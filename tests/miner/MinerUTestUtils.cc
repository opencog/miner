/*
 * opencog/tests/miner/MinerUTestUtils.h
 *
 * Copyright (C) 2018 SingularityNET Foundation
 * All Rights Reserved
 *
 * Written by Nil Geisweiller
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

#include "MinerUTestUtils.h"

#include <boost/range/algorithm/sort.hpp>

#include <opencog/util/random.h>
#include <opencog/util/algorithm.h>
#include <opencog/guile/SchemeSmob.h>
#include <opencog/atoms/core/LambdaLink.h>
#include <opencog/ure/forwardchainer/ForwardChainer.h>
#include <opencog/ure/backwardchainer/BackwardChainer.h>

using namespace opencog;

#define an as.add_node
#define al as.add_link

Handle MinerUTestUtils::add_db_cpt(AtomSpace& as)
{
	return an(CONCEPT_NODE, "db");
}

Handle MinerUTestUtils::add_minsup_prd(AtomSpace& as)
{
	return an(PREDICATE_NODE, "minsup");
}

Handle MinerUTestUtils::add_surp_prd(AtomSpace& as, std::string mode)
{
	return an(PREDICATE_NODE, std::move(mode));
}

Handle MinerUTestUtils::add_top(AtomSpace& as)
{
	Handle X = as.add_node(VARIABLE_NODE, "$X");
	return as.add_link(LAMBDA_LINK, X, MinerUtils::mk_body({X}));
}

Handle MinerUTestUtils::add_minsup_eval(AtomSpace& as,
                                        const Handle& pattern,
                                        int minsup,
                                        TruthValuePtr tv)
{
	Handle minsup_eval_h = al(EVALUATION_LINK,
	                          add_minsup_prd(as),
	                          al(LIST_LINK,
	                             pattern,
	                             add_db_cpt(as),
	                             an(NUMBER_NODE, std::to_string(minsup))));
	if (TruthValue::DEFAULT_TV() != tv)
		minsup_eval_h->setTruthValue(tv);
	return minsup_eval_h;
}

Handle MinerUTestUtils::add_minsup_evals(AtomSpace& as,
                                         const HandleSeq& patterns,
                                         int minsup,
                                         TruthValuePtr tv)
{
	HandleSeq minsup_evals;
	for (const Handle& pat : patterns)
		minsup_evals.push_back(add_minsup_eval(as, pat, minsup, tv));
	return al(SET_LINK, std::move(minsup_evals));
}

Handle MinerUTestUtils::add_surp_eval(AtomSpace& as,
                                       const std::string& mode,
                                       const Handle& pattern)
{
	Handle surp_eval_h = al(EVALUATION_LINK,
	                        add_surp_prd(as, mode),
	                        al(LIST_LINK,
	                           pattern,
	                           add_db_cpt(as)));
	return surp_eval_h;
}

Handle MinerUTestUtils::get_pattern(const Handle& minsup_eval)
{
	return minsup_eval->getOutgoingAtom(1)->getOutgoingAtom(0);
}

HandleSeq MinerUTestUtils::get_patterns(const HandleSeq& minsup_evals)
{
	HandleSeq patterns;
	for (const Handle& minsup_eval : minsup_evals)
		patterns.push_back(get_pattern(minsup_eval));
	return patterns;
}

Handle MinerUTestUtils::add_abs_true_eval(AtomSpace& as, const Handle& h)
{
	return al(EVALUATION_LINK,
	          an(GROUNDED_PREDICATE_NODE, "scm-eager: absolutely-true"),
	          h);
}

Handle MinerUTestUtils::add_nconjunct(AtomSpace& as, unsigned n)
{
	HandleSeq vars = add_variables(as, "$X-", n);
	HandleSeq varz = vars;
	return al(LAMBDA_LINK,
	          al(VARIABLE_SET, std::move(vars)),
	          al(PRESENT_LINK, std::move(varz)));
}

Handle MinerUTestUtils::add_variable(AtomSpace& as,
                                     const std::string& prefix,
                                     int i)
{
	return an(VARIABLE_NODE, prefix + std::to_string(i));
}

HandleSeq MinerUTestUtils::add_variables(AtomSpace& as,
                                         const std::string& prefix,
                                         int n)
{
	HandleSeq vars;
	for (int i = 0; i < n; i++)
		vars.push_back(add_variable(as, prefix, i));
	return vars;
}

Handle MinerUTestUtils::ure_pm(AtomSpace& as,
                               SchemeEval& scm,
                               const Handle& pm_rb,
                               const AtomSpace& db_as,
                               int minsup,
                               int maximum_iterations,
                               Handle initpat,
                               bool conjunction_expansion,
                               unsigned max_conjuncts,
                               unsigned max_variables,
                               unsigned max_spcial_conjuncts,
                               unsigned max_cnjexp_variables,
                               bool enforce_specialization,
                               double complexity_penalty,
                               bool enable_type,
                               bool enable_glob,
                               std::vector<std::string> ignore_vars)
{
	HandleSeq db;
	db_as.get_handles_by_type(db, opencog::ATOM, true);
	return ure_pm(as, scm, pm_rb, db, minsup, maximum_iterations, initpat,
	              conjunction_expansion, max_conjuncts, max_variables,
	              max_spcial_conjuncts, max_cnjexp_variables,
	              enforce_specialization, complexity_penalty,
	              enable_type, enable_glob, ignore_vars);
}

Handle MinerUTestUtils::ure_pm(AtomSpace& as,
                               SchemeEval& scm,
                               const Handle& pm_rb,
                               const HandleSeq& db,
                               int minsup,
                               int maximum_iterations,
                               Handle initpat,
                               bool conjunction_expansion,
                               unsigned max_conjuncts,
                               unsigned max_variables,
                               unsigned max_spcial_conjuncts,
                               unsigned max_cnjexp_variables,
                               bool enforce_specialization,
                               double complexity_penalty,
                               bool enable_type,
                               bool enable_glob,
                               std::vector<std::string> ignore_vars)
{
	// Make (Member dt (Concept "db)) links
	for (const Handle& dt : db)
		al(MEMBER_LINK, dt, add_db_cpt(as));

	// If initpat is not defined then use top
	if (not initpat)
		initpat = add_top(as);

	// Add a variable default declaration if needed
	initpat = add_default_vardecl(initpat);

	// Add the axiom that initpat has enough support, and use it as
	// source for the forward chainer
	bool es = MinerUtils::enough_support(initpat, db, minsup);

	// If it doesn't have enough support return the empty solution
	if (not es)
		return al(SET_LINK);

	// Add incremental conjunction expansion if necessary
	configure_optional_rules(scm,
	                         conjunction_expansion,
	                         max_conjuncts,
	                         max_variables,
	                         max_spcial_conjuncts,
	                         max_cnjexp_variables,
	                         enforce_specialization,
	                         enable_type,
	                         enable_glob,
	                         ignore_vars);

	// Otherwise prepare the source
	TruthValuePtr tv = TruthValue::TRUE_TV();
	Handle source = add_minsup_eval(as, initpat, minsup, tv);

	// Run the forward chainer from the initial pattern
	ForwardChainer fc(as, pm_rb, source);
	fc.get_config().set_maximum_iterations(maximum_iterations);
	fc.get_config().set_retry_exhausted_sources(conjunction_expansion);
	fc.get_config().set_complexity_penalty(complexity_penalty);
	fc.do_chain();

	// Run the pattern matcher query to gather the knowledge of
	// interest, i.e. patterns reaching the minimum support, and
	// return the results.
	Handle patvar = an(VARIABLE_NODE, "$patvar"),
		target = add_minsup_eval(as, patvar, minsup),
		vardecl = al(TYPED_VARIABLE_LINK, patvar, an(TYPE_NODE, "LambdaLink")),
		abs_true = add_abs_true_eval(as, target),
		bl = al(BIND_LINK, vardecl, al(AND_LINK, target, abs_true), target),
		results = HandleCast(bl->execute(&as));

	return results;
}

HandleTree MinerUTestUtils::cpp_pm(const AtomSpace& db_as,
                                   int minsup,
                                   int conjuncts,
                                   const Handle& initpat,
                                   int maxdepth)
{
	MinerParameters param(minsup, conjuncts, initpat, maxdepth);
	Miner pm(param);
	return pm(db_as);
}

HandleTree MinerUTestUtils::cpp_pm(const HandleSeq& db,
                                   int minsup,
                                   int conjuncts,
                                   const Handle& initpat,
                                   int maxdepth)
{
	MinerParameters param(minsup, conjuncts, initpat, maxdepth);
	Miner pm(param);
	return pm(db);
}

Handle MinerUTestUtils::add_is_cpt_pattern(AtomSpace& as, const Handle& cpt)
{
	Handle X = an(VARIABLE_NODE, "$X"),
		is_cpt = al(INHERITANCE_LINK, X, cpt),
		pattern = MinerUtils::mk_pattern(X, {is_cpt});
	return pattern;
}

Handle MinerUTestUtils::add_ugly_pattern(AtomSpace& as)
{
	return add_is_cpt_pattern(as, an(CONCEPT_NODE, "ugly"));
}

Handle MinerUTestUtils::add_man_pattern(AtomSpace& as)
{
	return add_is_cpt_pattern(as, an(CONCEPT_NODE, "man"));
}

Handle MinerUTestUtils::add_soda_drinker_pattern(AtomSpace& as)
{
	return add_is_cpt_pattern(as, an(CONCEPT_NODE, "soda drinker"));
}

Handle MinerUTestUtils::add_ugly_man_pattern(AtomSpace& as)
{
	Handle X = an(VARIABLE_NODE, "$X"),
		man = an(CONCEPT_NODE, "man"),
		ugly = an(CONCEPT_NODE, "ugly"),
		is_man = al(INHERITANCE_LINK, X, man),
		is_ugly = al(INHERITANCE_LINK, X, ugly),
		pattern = MinerUtils::mk_pattern(X, {is_ugly, is_man});
	return pattern;
}

Handle MinerUTestUtils::add_ugly_man_soda_drinker_pattern(AtomSpace& as)
{
	Handle X = an(VARIABLE_NODE, "$X"),
		man = an(CONCEPT_NODE, "man"),
		soda_drinker = an(CONCEPT_NODE, "soda drinker"),
		ugly = an(CONCEPT_NODE, "ugly"),
		is_man = al(INHERITANCE_LINK, X, man),
		is_soda_drinker = al(INHERITANCE_LINK, X, soda_drinker),
		is_ugly = al(INHERITANCE_LINK, X, ugly),
		pattern = MinerUtils::mk_pattern(X, {is_ugly, is_man, is_soda_drinker});
	return pattern;
}

void MinerUTestUtils::configure_mandatory_rules(SchemeEval& scm)
{
	std::string rs = scm.eval("(configure-mandatory-rules (Concept \"pm-rbs\"))");
	logger().debug() << "MinerUTest::configure_mandatory_rules() rs = " << rs;
}

inline std::string comb_vars(std::vector<std::string> vnames)
{
	std::string slink = "(SetLink ";
	for(auto vn : vnames)
		slink += "(VariableNode \"" + vn +"\") ";
	slink += ")";
	return slink;
}

void MinerUTestUtils::configure_optional_rules(SchemeEval& scm,
                                               bool conjunction_expansion,
                                               unsigned max_conjuncts,
                                               unsigned max_variables,
                                               unsigned max_spcial_conjuncts,
                                               unsigned max_cnjexp_variables,
                                               bool enforce_specialization,
                                               bool enable_type,
                                               bool enable_glob,
                                               std::vector<std::string> ignore_vars)
{
	std::string call = "(configure-optional-rules (Concept \"pm-rbs\")";
	call += " #:conjunction-expansion #";
	call += conjunction_expansion ? "t" : "f";
	call += " #:maximum-conjuncts ";
	call += std::to_string(max_conjuncts);
	call += " #:maximum-variables ";
	call += std::to_string(max_variables);
	call += " #:maximum-spcial-conjuncts ";
	call += std::to_string(max_spcial_conjuncts);
	call += " #:maximum-cnjexp-variables ";
	call += std::to_string(max_cnjexp_variables);
	call += " #:enforce-specialization ";
	call += enforce_specialization ? "#t" : "#f";
	call += " #:enable-type ";
	call += enable_type ? "#t" : "#f";
	call += " #:enable-glob ";
	call += enable_glob ? "#t" : "#f";
	call += " #:ignore-variables ";
	call += comb_vars(ignore_vars);
	call += ")";
	std::string rs = scm.eval(call);
	logger().debug() << "MinerUTest::configure_optional_rules() rs = " << rs;
}

void MinerUTestUtils::configure_surprisingness(SchemeEval& scm,
                                               const Handle& surp_rb,
                                               const std::string& mode,
                                               unsigned max_conjuncts,
                                               double db_ratio)
{
	std::string call = "(configure-surprisingness (Concept \""
		+ surp_rb->get_name() + "\") ";
	call += std::string("'") + mode + " ";
	call += std::to_string(max_conjuncts) + " ";
	call += std::to_string(db_ratio);
	call += ")";
	std::string rs = scm.eval(call);
	logger().debug() << "MinerUTest::configure_surprisingness() rs = " << rs;
}

HandleSeq MinerUTestUtils::ure_surp(AtomSpace& as,
                                    SchemeEval& scm,
                                    const Handle& surp_rb,
                                    const std::string& mode,
                                    unsigned max_conjuncts,
                                    double db_ratio)
{
	configure_surprisingness(scm, surp_rb, mode, max_conjuncts, db_ratio);
	Handle X = an(VARIABLE_NODE, "$X"),
		target = add_surp_eval(as, mode, X),
		vardecl = al(TYPED_VARIABLE_LINK, X, an(TYPE_NODE, "LambdaLink"));
	BackwardChainer bc(as, surp_rb, target, vardecl);
	bc.do_chain();
	Handle surp_results = bc.get_results();
	HandleSeq surp_results_seq = surp_results->getOutgoingSet();
	// Sort according to surprisingness
	boost::sort(surp_results_seq, [](const Handle& lh, const Handle& rh) {
			return lh->getTruthValue()->get_mean() > rh->getTruthValue()->get_mean();
		});
	return surp_results_seq;
}

HandleSeq MinerUTestUtils::populate_nodes(AtomSpace& as,
                                          unsigned n,
                                          Type type,
                                          const std::string& prefix)
{
	// Create nodes i for i in [0, n)
	HandleSeq nodes(n);
	for (unsigned i = 0; i < n; i++)
		nodes[i] = as.add_node(type, prefix + std::to_string(i));
	return nodes;
}

HandleSeq MinerUTestUtils::populate_links(AtomSpace& as,
                                          const HandleSeq& hs,
                                          Type type,
                                          unsigned arity,
                                          double p)
{
	// Use set to ignore duplicate in case type is unordered
	HandleSet links;
	for (const HandleSeq& outgoing : cartesian_product(hs, arity))
		if (biased_randbool(p))
			links.insert(as.add_link(type, std::move(HandleSeq(outgoing))));
	return HandleSeq(links.begin(), links.end());
}

Handle MinerUTestUtils::add_default_vardecl(const Handle& pattern)
{
	if (1 < pattern->get_arity())
		return pattern;
	Handle vardecl = ScopeLinkCast(pattern)->get_variables().get_vardecl();
	Handle body = pattern->getOutgoingAtom(0);
	return HandleCast(createLambdaLink(vardecl, body));
}
