/*
 * Surprisingness.cc
 *
 * Copyright (C) 2019 OpenCog Foundation
 *
 * Author: Nil Geisweiller
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the exceptions
 * at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include "Surprisingness.h"

#include "MinerUtils.h"
#include "MinerLogger.h"

#include <opencog/util/Logger.h>
#include <opencog/util/lazy_random_selector.h>
#include <opencog/util/random.h>
#include <opencog/util/dorepeat.h>
#include <opencog/util/algorithm.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/core/FindUtils.h>
#include <opencog/atoms/core/LambdaLink.h>
#include <opencog/atoms/truthvalue/SimpleTruthValue.h>
#include <opencog/ure/BetaDistribution.h>

#include <boost/range/adaptor/transformed.hpp>
#include <boost/range/algorithm/transform.hpp>
#include <boost/range/algorithm/min_element.hpp>
#include <boost/range/algorithm/sort.hpp>
#include <boost/range/numeric.hpp>
#include <boost/math/special_functions/binomial.hpp>

#include <cmath>
#include <functional>
#include <limits>

namespace opencog {

double Surprisingness::isurp_old(const Handle& pattern,
                                 const HandleSeq& db,
                                 bool normalize)
{
	// Strictly speaking it should be the power but we use binomial for
	// backward compatibility.
	unsigned int n = db.size(), k = MinerUtils::n_conjuncts(pattern);
	double total_count = boost::math::binomial_coefficient<double>(n, k);

	// Function calculating the probability of a pattern
	auto prob = [&](const Handle& pattern) {
		double sup = MinerUtils::support(pattern, db, (unsigned)total_count);
		return sup / total_count;
	};
	auto blk_prob = [&](const HandleSeq& block) {
		return prob(add_pattern(block, *pattern->getAtomSpace()));
	};

	// Calculate the probability of pattern
	double pattern_prob = prob(pattern);

	// Calculate the probability estimate of each partition based on
	// independent assumption of between each partition block.
	auto iprob = [&](const HandleSeqSeq& partition) {
		return boost::accumulate(partition | boost::adaptors::transformed(blk_prob),
		                         1.0, std::multiplies<double>());
	};
	HandleSeqSeqSeq prtns = MinerUtils::partitions_without_pattern(pattern);
	std::vector<double> estimates(prtns.size());
	boost::transform(prtns, estimates.begin(), iprob);
	auto p = std::minmax_element(estimates.begin(), estimates.end());
	double emin = *p.first, emax = *p.second;

	// Calculate the I-Surprisingness, normalized if requested.
	double dst = dst_from_interval(emin, emax, pattern_prob);
	return std::min(normalize? dst / pattern_prob : dst, 1.0);
}

double Surprisingness::isurp(const Handle& pattern,
                             const HandleSeq& db,
                             bool normalize,
                             double db_ratio)
{
	// Calculate the probability estimate of each partition based on
	// independent assumption of between each partition block, taking
	// into account the linkage probability.
	auto [emin, emax] = ji_prob_est_interval(pattern, db, db_ratio);

	// Calculate the empirical probability of pattern, using
	// boostrapping if necessary
	double emp = emp_prob_pbs_mem(pattern, db, emax, db_ratio);

	// Calculate the I-Surprisingness, normalized if requested.
	double dst = dst_from_interval(emin, emax, emp);
	double maxprb = std::max(emp, emax);
	return std::min(normalize ? dst / maxprb : dst, 1.0);
}

double Surprisingness::dst_from_interval(double l, double u, double v)
{
	return (u < v ? v - u : (v < l ? l - v : 0.0));
}

Handle Surprisingness::add_pattern(const HandleSeq& block, AtomSpace& as)
{
	return as.add_link(LAMBDA_LINK, MinerUtils::mk_body(block));
}

HandleSeq Surprisingness::add_subpatterns(const HandleSeqSeq& partition,
                                          const Handle& pattern,
                                          AtomSpace& as)
{
	HandleSeq subpatterns(partition.size());
	boost::transform(partition, subpatterns.begin(), [&](const HandleSeq& blk) {
			return add_pattern(blk, as); });
	return subpatterns;
}

HandleSeq Surprisingness::joint_variables(const Handle& pattern,
                                          const HandleSeqSeq& partition)
{
	HandleUCounter var_count;

	for (const Handle& var : MinerUtils::get_variables(pattern).varset)
		for (const HandleSeq& blk : partition)
			if (is_free_in_any_tree(blk, var))
				++var_count[var];

	HandleSeq jvs;
	for (const auto& vc : var_count)
		if (1 < vc.second)
			jvs.push_back(vc.first);

	return jvs;
}

unsigned Surprisingness::value_count(const HandleSeq& block,
                                     const Handle& var,
                                     const HandleSeq& db)
{
	Valuations vs(MinerUtils::mk_pattern_no_vardecl(block), db);
	HandleUCounter values = vs.values(var);
	return values.keys().size();
}

HandleCounter Surprisingness::value_distribution(const HandleSeq& block,
                                                 const Handle& var,
                                                 const HandleSeq& db)
{
	Valuations vs(MinerUtils::mk_pattern_no_vardecl(block), db);
	HandleUCounter values = vs.values(var);
	HandleCounter dist;
	double total = values.total_count();
	for (const auto& v : values)
		dist[v.first] = v.second / total;
	return dist;
}

double Surprisingness::inner_product(const std::vector<HandleCounter>& dists)
{
	// Find the common intersection of values
	HandleSet cvals = dists[0].keys();
	for (size_t i = 1; i < dists.size(); i++)
		cvals = opencog::set_intersection(cvals, dists[i].keys());

	// Calculate the inner product of all distributions across the
	// common values
	double p = 0.0;
	for (const Handle& v : cvals) {
		double inner = 1.0;
		for (const HandleCounter& dist : dists)
			inner *= dist.at(v);
		p += inner;
	}
	return p;
}

double Surprisingness::universe_count(const Handle& pattern,
                                      const HandleSeq& db)
{
	return std::pow((double)db.size(), MinerUtils::n_conjuncts(pattern));
}

double Surprisingness::prob_to_support(const Handle& pattern,
                                       const HandleSeq& db,
                                       double prob)
{
	return prob * universe_count(pattern, db);
}

double Surprisingness::emp_prob(const Handle& pattern, const HandleSeq& db)
{
	double ucount = universe_count(pattern, db);
	unsigned ms = (unsigned)std::min((double)UINT_MAX, ucount);
	double sup = MinerUtils::support(pattern, db, ms);
	return sup / ucount;
}

double Surprisingness::emp_prob_mem(const Handle& pattern, const HandleSeq& db)
{
	TruthValuePtr emp_prob_tv = get_emp_tv(pattern);
	if (emp_prob_tv) {
		return emp_prob_tv->get_mean();
	}
	double ep = emp_prob(pattern, db);
	set_emp_prob(pattern, ep);
	return ep;
}

double Surprisingness::emp_prob_subsmp(const Handle& pattern,
                                       const HandleSeq& db,
                                       unsigned subsize)
{
	return emp_prob(pattern,
	                subsize < db.size() ?
	                subsmp(db, subsize) : db);
}

TruthValuePtr Surprisingness::emp_tv(const Handle& pattern, const HandleSeq& db)
{
	double ucount = universe_count(pattern, db);
	unsigned ms = (unsigned)std::min((double)UINT_MAX, ucount);
	double sup = MinerUtils::support(pattern, db, ms);
	double mean = sup / ucount;
	double conf = count_to_confidence(ucount);
	// Hack alert! Lower the confidence because subsampling can
	// introduce some erratic errors.
	conf *= 1e-1;
	return createSimpleTruthValue(mean, conf);
}

TruthValuePtr Surprisingness::emp_tv_mem(const Handle& pattern,
                                         const HandleSeq& db)
{
	TruthValuePtr etv = get_emp_tv(pattern);
	if (etv) {
		return etv;
	}
	etv = emp_tv(pattern, db);
	set_emp_tv(pattern, etv);
	return etv;
}

TruthValuePtr Surprisingness::emp_tv_subsmp(const Handle& pattern,
                                            const HandleSeq& db,
                                            unsigned subsize)
{
	return emp_tv(pattern,
	              subsize < db.size() ?
	              subsmp(db, subsize) : db);
}

double Surprisingness::emp_prob_bs(const Handle& pattern,
                                   const HandleSeq& db,
                                   unsigned n_resample,
                                   unsigned subsize)
{
	if (subsize < db.size()) {
		std::vector<double> essprobs;
		dorepeat(n_resample)
			essprobs.push_back(emp_prob_subsmp(pattern, db, subsize));
		return avrg(essprobs);
	} else {
		return emp_prob(pattern, db);
	}
}

double Surprisingness::emp_prob_pbs(const Handle& pattern,
                                    const HandleSeq& db,
                                    double db_ratio)
{
	if (1 < MinerUtils::n_conjuncts(pattern)) {
		// If there is more than one conjunct, calculate an estimate
		// first to subsample the db if necessary
		auto [emin, emax] = ji_prob_est_interval(pattern, db, db_ratio);
		return emp_prob_pbs(pattern, db, emax, db_ratio);
	} else {
		// Otherwise, no subsampling is necessary, should be tractable
		return emp_prob(pattern, db);
	}
}

double Surprisingness::emp_prob_pbs(const Handle& pattern,
                                    const HandleSeq& db,
                                    double prob_estimate,
                                    double db_ratio)
{
	// Calculate an estimate of the support of the pattern to decide
	// whether we should subsample the db corpus. Indeed some
	// patterns have intractably large support.
	double support_estimate = prob_to_support(pattern, db, prob_estimate);
	double db_size = db.size() * db_ratio;

	// If the support estimate is above the db size then we
	// subsample
	if (db_size < support_estimate) {
		LAZY_MINER_LOG_FINE << "Pattern" << std::endl << oc_to_string(pattern)
		              << std::endl << "has support estimate " << support_estimate
		              << " > " << db_size << " (its rescaled db size)";
		// Calculate the empirical probability of pattern
		unsigned subsize = subsmp_size(pattern, db_size, support_estimate);
		unsigned n_resample = 10;  // TODO: move this hardwired contant
		                             // to a user parameter
		LAZY_MINER_LOG_FINE << "Downsample the db to " << subsize
		              << " to avoid excessively large support,"
		              << " boostrapping" << " (x" << n_resample << ")"
		              << " to reduce inaccuracies.";
		double emp_prob = emp_prob_bs(pattern, db, n_resample, subsize);
		if (emp_prob == 0) {
			LAZY_MINER_LOG_WARN << "The empirical probability of pattern" << std::endl
			              << oc_to_string(pattern) << std::endl
			              << "is null. You probably want to increase the db-ratio"
			              << ", currently of " << db_ratio;
		}
		return emp_prob;
	} else {
		LAZY_MINER_LOG_FINE << "Pattern " << std::endl << oc_to_string(pattern)
		              << std::endl << "has support estimate " << support_estimate
		              << " <= " << db_size << " (its rescaled db size)"
		              << ". No subsampling is taking place.";
		return emp_prob(pattern, db);
	}
}

double Surprisingness::emp_prob_pbs_mem(const Handle& pattern,
                                        const HandleSeq& db,
                                        double db_ratio)
{
	TruthValuePtr etv = get_emp_tv(pattern);
	if (etv) {
		return etv->get_mean();
	}
	double ep = emp_prob_pbs(pattern, db, db_ratio);
	set_emp_prob(pattern, ep);
	return ep;
}

double Surprisingness::emp_prob_pbs_mem(const Handle& pattern,
                                        const HandleSeq& db,
                                        double prob_estimate,
                                        double db_ratio)
{
	TruthValuePtr etv = get_emp_tv(pattern);
	if (etv) {
		return etv->get_mean();
	}
	double ep = emp_prob_pbs(pattern, db, prob_estimate, db_ratio);
	set_emp_prob(pattern, ep);
	return ep;
}

TruthValuePtr Surprisingness::emp_tv_bs(const Handle& pattern,
                                        const HandleSeq& db,
                                        unsigned n_resample,
                                        unsigned subsize)
{
	if (subsize < db.size()) {
		TruthValueSeq esstvs;
		dorepeat(n_resample)
			esstvs.push_back(emp_tv_subsmp(pattern, db, subsize));
		return avrg_tv(esstvs);
	} else {
		TruthValuePtr etv = emp_tv(pattern, db);
		return etv;
	}
}

TruthValuePtr Surprisingness::emp_tv_pbs(const Handle& pattern,
                                         const HandleSeq& db,
                                         double prob_estimate,
                                         double db_ratio)
{
	// Calculate an estimate of the support of the pattern to decide
	// whether we should subsample the db corpus. Indeed some
	// pattern have intractably large support.
	double support_estimate = prob_to_support(pattern, db, prob_estimate);
	double db_size = db.size() * db_ratio;

	// If the support estimate is above the db size then we
	// subsample
	if (db_size < support_estimate) {
		// Calculate the empirical probability of pattern
		unsigned subsize = subsmp_size(pattern, db_size, support_estimate);
		unsigned n_resample = 10;  // TODO: move this hardwired contant
		                             // to a user parameter
		return emp_tv_bs(pattern, db, n_resample, subsize);
	} else {
		return emp_tv(pattern, db);
	}
}

TruthValuePtr Surprisingness::emp_tv_pbs_mem(const Handle& pattern,
                                             const HandleSeq& db,
                                             double prob_estimate,
                                             double db_ratio)
{
	TruthValuePtr etv = get_emp_tv(pattern);
	if (etv) {
		return etv;
	}
	etv = emp_tv_pbs(pattern, db, prob_estimate, db_ratio);
	set_emp_tv(pattern, etv);
	return etv;
}

HandleSeq Surprisingness::subsmp(const HandleSeq& db, unsigned subsize)
{
	unsigned ts = db.size();
	if (ts/2 <= subsize and subsize < ts) {
		// Subsample by randomly removing (swapping all elements to
		// remove with the tail, then removing the tail, which is
		// considerably faster than removing element by element).
		HandleSeq smp_db(db);
		unsigned i = ts;
		while (subsize < i) {
			unsigned rnd_idx = randGen().randint(i);
			std::swap(smp_db[rnd_idx], smp_db[--i]);
		}
		smp_db.resize(i);
		return smp_db;
	} else if (0 <= subsize and subsize < ts/*/2*/) {
		// Subsample by randomly adding
		HandleSeq smp_db(subsize);
		lazy_random_selector select(ts);
		for (size_t i = 0; i < subsize; i++)
			smp_db[i] = db[select()];
		return smp_db;
	} else {
		return db;
	}
}

unsigned Surprisingness::subsmp_size(const Handle& pattern,
                                     double db_size,
                                     double support_estimate,
                                     unsigned min_subsize)
{
	double nc = MinerUtils::n_conjuncts(pattern);
	double alpha = support_estimate / std::pow(db_size, nc);
	double res = std::pow(db_size / (10*alpha), 1.0/nc);
	return std::max((unsigned)res, std::min(min_subsize, (unsigned)db_size));
}

std::pair<double, double> Surprisingness::ji_prob_est_interval(const Handle& pattern,
                                                               const HandleSeq& db,
                                                               double db_ratio)
{
	// Calculate the probability estimate of each partition based on
	// independent assumption of between each partition block, taking
	// into account the linkage probability.
	std::vector<double> estimates;
	HandleSeqSeqSeq prtns = MinerUtils::partitions_without_pattern(pattern);
	for (const HandleSeqSeq& partition : prtns) {
		double jip = ji_prob_est(partition, pattern, db, db_ratio);
		estimates.push_back(jip);
	}
	auto mmp = std::minmax_element(estimates.begin(), estimates.end());
	double emin = *mmp.first, emax = *mmp.second;

	return {emin, emax};
}

double Surprisingness::ji_prob_est(const HandleSeqSeq& partition,
                                   const Handle& pattern,
                                   const HandleSeq& db,
                                   double db_ratio)
{
	// Generate subpatterns from blocks (add them in the atomspace to
	// memoize support calculation)
	HandleSeq subpatterns = add_subpatterns(partition, pattern,
	                                        *pattern->getAtomSpace());

	// Calculate the product of the probability over subpatterns
	// without considering joint variables
	double p = 1.0;
	for (const Handle& subpattern : subpatterns) {
		double empr = emp_prob_pbs_mem(subpattern, db, db_ratio);
		p *= empr;
	}

	// Calculate the probability that all joint variables take the same
	// value
	double eq_p = eq_prob(partition, pattern, db);
	p *= eq_p;

	return p;
}

TruthValuePtr Surprisingness::ji_tv_est(const HandleSeqSeq& partition,
                                        const Handle& pattern,
                                        const HandleSeq& db)
{
	// Generate subpatterns from blocks (add them in the atomspace to
	// memoize support calculation)
	HandleSeq subpatterns = add_subpatterns(partition, pattern,
	                                        *pattern->getAtomSpace());

	// Calculate the product of the probability over subpatterns
	// without considering joint variables
	double rp = 1.0;
	double rc = 1.0;
	TruthValueSeq etvs;
	for (const Handle& subpattern : subpatterns) {
		TruthValuePtr etv = emp_tv_mem(subpattern, db);
		etvs.push_back(etv);
	}
	TruthValuePtr prod_etv = avrg_tv(etvs);
	rp = prod_etv->get_mean();
	rc = prod_etv->get_confidence();

	// Calculate the probability that all joint variables take the same
	// value
	double eq_p = eq_prob(partition, pattern, db);
	rp *= eq_p;
	// Hack alert! Lower the confidence because it is an estimate
	// after all.
	rc *= 1e-1;

	return createSimpleTruthValue(rp, rc);
}

TruthValuePtr Surprisingness::ji_tv_est(const Handle& pattern,
                                        const HandleSeq& db)
{
	// Calculate the truth value estimate of each partition based on
	// independent assumption of between each partition block, taking
	// into account the linkage probability.
	TruthValueSeq etvs;
	HandleSeqSeqSeq prtns = MinerUtils::partitions_without_pattern(pattern);
	for (const HandleSeqSeq& partition : prtns) {
		TruthValuePtr etv = ji_tv_est(partition, pattern, db);
		etvs.push_back(etv);
	}
	return avrg_tv(etvs);
}

TruthValuePtr Surprisingness::ji_tv_est_mem(const Handle& pattern,
                                            const HandleSeq& db)
{
	TruthValuePtr jte = get_ji_tv_est(pattern);
	if (jte) {
		return jte;
	}
	jte = ji_tv_est(pattern, db);
	set_ji_tv_est(pattern, jte);
	return jte;
}

bool Surprisingness::has_same_index(const Handle& l_pat,
                                    const Handle& r_pat,
                                    const Handle& var)
{
	const Variables& lv = LambdaLinkCast(l_pat)->get_variables();
	const Variables& rv = LambdaLinkCast(r_pat)->get_variables();
	auto lv_it = lv.index.find(var);
	auto rv_it = rv.index.find(var);
	return lv_it != lv.index.end() and rv_it != lv.index.end()
		and lv_it->second == rv_it->second;
}

bool Surprisingness::is_equivalent(const HandleSeq& l_blk,
                                   const HandleSeq& r_blk,
                                   const Handle& var)
{
	Handle l_pat = MinerUtils::mk_pattern_no_vardecl(l_blk);
	Handle r_pat = MinerUtils::mk_pattern_no_vardecl(r_blk);
	return is_equivalent(l_pat, r_pat, var);
}

bool Surprisingness::is_equivalent(const Handle& l_pat,
                                   const Handle& r_pat,
                                   const Handle& var)
{
	return content_eq(l_pat, r_pat) and has_same_index(l_pat, r_pat, var);
}

bool Surprisingness::is_strictly_more_abstract(const HandleSeq& l_blk,
                                               const HandleSeq& r_blk,
                                               const Handle& var)
{
	return not is_equivalent(l_blk, r_blk, var)
		and MinerUtils::is_blk_more_abstract(l_blk, r_blk, var);
}

void Surprisingness::rank_by_abstraction(HandleSeqSeq& partition, const Handle& var)
{
	boost::sort(partition,
	            [&](const HandleSeq& l_blk, const HandleSeq& r_blk) {
		            // sort operates on strict weak order so is
		            // compatible with is_strictly_more_abstract
		            return is_strictly_more_abstract(l_blk, r_blk, var);
	            });
}

double Surprisingness::eq_prob(const HandleSeqSeq& partition,
                               const Handle& pattern,
                               const HandleSeq& db)
{
	double p = 1.0;
	// Calculate the probability of a variable taking the same value
	// across all blocks/subpatterns where that variable appears.
	for (const Handle& var : joint_variables(pattern, partition)) {

		// Select all strongly connected subpatterns containing var
		HandleSeqSeq var_partition =
			MinerUtils::connected_subpatterns_with_var(partition, var);

		// For each variable, sort the partition so that abstract
		// blocks, relative to var, appear first.
		rank_by_abstraction(var_partition, var);

		// For each block j_blk, but the first, look for the most
		// specialized block that is more abstract or equivalent to that
		// block relative to var, i_blk, and use the fact that var
		// cannot take more values than its count in i_blk. If no such
		// i_blk block exists, then use uc=|U| as count.
		for (int j = 1; j < (int)var_partition.size(); j++) {
			// Since abstraction relation is transitive, one can just go
			// backward from j_blk and pick up the first i_blk that is
			// either equivalent or more abstract, it will be the most
			// specialized abstraction.
			int i = j-1;
			while (0 <= i)
				if (MinerUtils::is_blk_more_abstract(var_partition[i],
				                                     var_partition[j],
				                                     var))
					break;
				else i--;

			double c = db.size();
			if (0 <= i)
				c = value_count(var_partition[i], var, db);
			p /= c;
		}
	}
	return p;
}

const Handle& Surprisingness::emp_tv_key()
{
	static Handle etvk(createNode(NODE, "*-EmpiricalTruthValueKey-*"));
	return etvk;
}

TruthValuePtr Surprisingness::get_emp_tv(const Handle& pattern)
{
	ValuePtr val = pattern->getValue(emp_tv_key());
	if (val)
		return TruthValueCast(val);
	return nullptr;
}

void Surprisingness::set_emp_tv(const Handle& pattern, TruthValuePtr etv)
{
	pattern->setValue(emp_tv_key(), ValueCast(etv));
}

void Surprisingness::set_emp_prob(const Handle& pattern, double ep)
{
	TruthValuePtr etv = createSimpleTruthValue(ep, 1.0);
	set_emp_tv(pattern, etv);
}

const Handle& Surprisingness::ji_tv_est_key()
{
	static Handle jtek(createNode(NODE, "*-JointIndependentTruthValueEstimateKey-*"));
	return jtek;
}

TruthValuePtr Surprisingness::get_ji_tv_est(const Handle& pattern)
{
	ValuePtr val = pattern->getValue(ji_tv_est_key());
	if (val)
		return TruthValueCast(val);
	return nullptr;
}

void Surprisingness::set_ji_tv_est(const Handle& pattern, TruthValuePtr jte)
{
	pattern->setValue(ji_tv_est_key(), ValueCast(jte));
}

double Surprisingness::jsd(TruthValuePtr l_tv, TruthValuePtr r_tv)
{
	static int bins = 100;
	BetaDistribution l_bd(l_tv);
	BetaDistribution r_bd(r_tv);
	std::vector<double>
		l_cdf = l_bd.cdf(bins),
		r_cdf = r_bd.cdf(bins),
		m_cdf = avrg_cdf(l_cdf, r_cdf);
	double
		ld = kld(l_cdf, m_cdf),
		rd = kld(r_cdf, m_cdf);

	// logger().debug() << "CSV representation of the pdf of the left TV. ";
	// log_pdf(l_bd, bins);
	// logger().debug() << "CSV representation of the pdf of the right TV. ";
	// log_pdf(r_bd, bins);

	return sqrt(avrg(ld, rd));
}

double Surprisingness::kld(const std::vector<double>& l_cdf,
                           const std::vector<double>& r_cdf)
{
	static double epsilon = 1e-32;
	OC_ASSERT(l_cdf.size() == r_cdf.size());

	// Value of the previous data point in the left and right cdf
	// respectively
	double last_lv = 0.0;
	double last_rv = 0.0;

	// Integrate the relative entropy between the 2 cdfs for each data
	// point
	double kldi = 0.0;
	for (size_t i = 0; i < l_cdf.size(); i++) {
		// Probabilities of the right and left points
		double lp = l_cdf[i] - last_lv;
		double rp = r_cdf[i] - last_rv;
		// Relative entropy
		if (epsilon < rp and epsilon < lp) {
			double e = lp * std::log2(lp/rp);
			kldi += e;
		}
		// Remember last cummulated probabilities
		last_lv = l_cdf[i];
		last_rv = r_cdf[i];
	}
	return kldi;
}

double Surprisingness::avrg(double l, double r)
{
	return (l + r) / 2.0;
}

double Surprisingness::avrg(std::vector<double>& vs)
{
	return boost::accumulate(vs, 0.0) / vs.size();
}

TruthValuePtr Surprisingness::avrg_tv(const TruthValueSeq& tvs)
{
	// Calculate the TV means and variances
	std::vector<BetaDistribution> dists;
	std::vector<double> means, variances;
	boost::transform(tvs, std::back_inserter(dists), mk_beta_distribution);
	boost::transform(dists, std::back_inserter(means),
	                 [](const BetaDistribution& bd) { return bd.mean(); });
	boost::transform(tvs, std::back_inserter(variances),
	                 [](const BetaDistribution& bd) { return bd.variance(); });

	// For now the mixed TV remains a SimpleTV, thus a
	// beta-distribution. The mean and variance is calculated
	// according to
	// https://en.wikipedia.org/wiki/Mixture_distribution#Moments
	double mean = avrg(means);
	std::vector<double> relative_variances(variances);
	for (std::size_t i = 0; i < relative_variances.size(); i++)
		relative_variances[i] += sq(means[i] - mean);
	double variance = avrg(relative_variances);

	return mk_stv(mean, variance);
}

std::vector<double> Surprisingness::avrg_cdf(const std::vector<double>& l_cdf,
                                             const std::vector<double>& r_cdf)
{
	OC_ASSERT(l_cdf.size() == r_cdf.size());
	std::vector<double> m_cdf(l_cdf.size());
	boost::transform(l_cdf, r_cdf, m_cdf.begin(),
	                 [](double l, double r) { return avrg(l, r); });
	return m_cdf;
}

count_t Surprisingness::confidence_to_count(confidence_t cfd)
{
	return cfd * SimpleTruthValue::DEFAULT_K / (1.0 - cfd);
}

confidence_t Surprisingness::count_to_confidence(count_t cnt)
{
	return cnt / (cnt + SimpleTruthValue::DEFAULT_K);
}

void Surprisingness::log_pdf(const BetaDistribution& bd, int bins)
{
	logger().debug() << "Paste the following in a file 'pdf.csv':" << std::endl
	                 << "# probability,density" << std::endl
	                 << bd.pdf_csv(bins);
	logger().debug() << "Then use the following gnuplot commands to plot it:" << std::endl
	                 << "set datafile separator comma" << std::endl
	                 << "plot 'pdf.csv' using 1:2;";
}

} // namespace opencog
