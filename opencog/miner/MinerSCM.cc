/*
 * MinerSCM.cc
 *
 * Copyright (C) 2018 OpenCog Foundation
 *
 * Author: Nil Geisweiller <ngeiswei@gmail.com> 
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

#ifdef HAVE_GUILE

#include <cmath>

#include <opencog/util/Logger.h>
#include <opencog/guile/SchemeModule.h>
#include <opencog/atoms/core/NumberNode.h>

#include "MinerUtils.h"
#include "Surprisingness.h"
#include "MinerLogger.h"

namespace opencog {

class MinerSCM : public ModuleWrap
{
protected:
	virtual void init();

	/**
	 * Given a pattern, a db concept and a minimum support, return
	 * all shallow abstractions reaching the minimum support,
	 * pre-wrapped in Lists ready to be applied to the pattern.
	 *
	 * For instance, given
	 *
	 * pattern = (Lambda X Y (Inheritance X Y))
	 * db  = { (Inheritance A B),
	 *         (Inheritance A C),
	 *         (Inheritance D D),
	 *         (Inheritance E E) }
	 * ms = (Number 2)
	 *
	 * returns
	 *
	 * Set
	 *   List A Y
	 *   List Y Y
	 */
	Handle do_shallow_abstract(Handle pattern, Handle db, Handle ms);

	/**
	 * Given a pattern, a db concept and a minimum support, return
	 * all shallow specializations reaching the minimum support.
	 *
	 * For instance, given
	 *
	 * pattern = (Lambda X Y (Inheritance X Y))
	 * db  = { (Inheritance A B),
	 *         (Inheritance A C),
	 *         (Inheritance D D),
	 *         (Inheritance E E) }
	 * ms = (Number 2)
	 *
	 * returns
	 *
	 * (Set
	 *   (Lambda Y (Inheritance A Y))
	 *   (Lambda Y (Inheritance Y Y)))
	 */
	Handle do_shallow_specialize(Handle pattern, Handle db,
	                             Handle ms, Handle mv,
	                             Handle enable_type,
	                             Handle enable_glob,
	                             Handle ignore_vars);

	/**
	 * Given a pattern, a db concept and a minimum support, return
	 * true iff the pattern has enough support.
	 */
	bool do_enough_support(Handle pattern, Handle db, Handle ms);

	/**
	 * Construct the conjunction of 2 patterns. If cnjtion is a
	 * conjunction, then expand it with pattern. It is assumed that
	 * pattern cannot be a conjunction itself.
	 *
	 * ms is the minimum support
	 * mv is the maximum of variables
	 * es is a flag to enforce specialization
	 */
	Handle do_expand_conjunction(Handle cnjtion, Handle pattern, Handle db,
	                             Handle ms, Handle mv, bool es);

	/**
	 * Calculate the I-Surprisingness of the pattern (and its
	 * partitions) with respect to db.
	 *
	 * do_isurp_old: Shujing I-Surprisingness
	 * do_nisurp_old: Shujing normalized I-Surprisingness
	 * do_isurp: I-Surprisingness
	 * do_nisurp: normalized I-Surprisingness
	 */
	double do_isurp_old(Handle pattern, Handle db, Handle /*db_ratio unused*/);
	double do_nisurp_old(Handle pattern, Handle db, Handle /*db_ratio unused*/);
	double do_isurp(Handle pattern, Handle db, Handle db_ratio);
	double do_nisurp(Handle pattern, Handle db, Handle db_ratio);

	/**
	 * Calculate the empirical truth value of pattern
	 */
	TruthValuePtr do_emp_tv(Handle pattern, Handle db, Handle db_ratio);

	/**
	 * Calculate the joint independent truth value estimate of pattern
	 */
	TruthValuePtr do_ji_tv_est(Handle pattern, Handle db);

	/**
	 * Calculate the Jensen-Shannon distance between 2 truth values
	 */
	double do_jsd(TruthValuePtr ltv, TruthValuePtr rtv);

	/**
	 * Return the Miner logger
	 */
	Logger* do_miner_logger();

public:
	MinerSCM();
};

} /*end of namespace opencog*/

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/guile/SchemePrimitive.h>

#include <opencog/miner/Miner.h>

using namespace opencog;

MinerSCM::MinerSCM() : ModuleWrap("opencog miner") {}

/// This is called while (opencog miner) is the current
/// module.  Thus, all the definitions below happen in that module.
void MinerSCM::init(void)
{
	define_scheme_primitive("cog-shallow-abstract",
		&MinerSCM::do_shallow_abstract, this, "miner");

	define_scheme_primitive("cog-shallow-specialize",
		&MinerSCM::do_shallow_specialize, this, "miner");

	define_scheme_primitive("cog-enough-support?",
		&MinerSCM::do_enough_support, this, "miner");

	define_scheme_primitive("cog-expand-conjunction",
		&MinerSCM::do_expand_conjunction, this, "miner");

	define_scheme_primitive("cog-isurp-old",
		&MinerSCM::do_isurp_old, this, "miner");

	define_scheme_primitive("cog-nisurp-old",
		&MinerSCM::do_nisurp_old, this, "miner");

	define_scheme_primitive("cog-isurp",
		&MinerSCM::do_isurp, this, "miner");

	define_scheme_primitive("cog-nisurp",
		&MinerSCM::do_nisurp, this, "miner");

	define_scheme_primitive("cog-emp-tv",
		&MinerSCM::do_emp_tv, this, "miner");

	define_scheme_primitive("cog-ji-tv-est",
		&MinerSCM::do_ji_tv_est, this, "miner");

	define_scheme_primitive("cog-jsd",
		&MinerSCM::do_jsd, this, "miner");

	define_scheme_primitive("cog-miner-logger",
		&MinerSCM::do_miner_logger, this, "miner");
}

Handle MinerSCM::do_shallow_abstract(Handle pattern,
                                     Handle db,
                                     Handle ms_h)
{
	AtomSpacePtr asp = SchemeSmob::ss_get_env_as("cog-shallow-abstract");

	// Fetch data trees
	HandleSeq db_seq = MinerUtils::get_db(db);

	// Fetch the minimum support
	unsigned ms = MinerUtils::get_uint(ms_h);

	// Generate all shallow abstractions
	HandleSetSeq shabs_per_var =                 // TODO add type and glob params.
		MinerUtils::shallow_abstract(pattern, db_seq, ms, false, false, {});

	// Turn that sequence of handle sets into a set of ready to be
	// applied shallow abstractions
	const Variables& vars = MinerUtils::get_variables(pattern);
	HandleSet sa_lists;
	unsigned vari = 0;         // Index of the variable
	for (const HandleSet& shabs : shabs_per_var) {
		for (const Handle& sa : shabs) {
			HandleSeq sa_list = vars.varseq;
			sa_list[vari] = sa;
			sa_lists.insert(sa_list.size() == 1 ? sa_list[0]
			                // Only Wrap in a list if arity is greater
			                // than one
			                : asp->add_link(LIST_LINK, std::move(sa_list)));
		}
		vari++;
	}

	return asp->add_link(SET_LINK, HandleSeq(sa_lists.begin(), sa_lists.end()));
}

Handle MinerSCM::do_shallow_specialize(Handle pattern,
                                       Handle db,
                                       Handle ms_h,
                                       Handle mv_h,
                                       Handle enable_type,
                                       Handle enable_glob,
                                       Handle ignore_vars)
{
	AtomSpacePtr asp = SchemeSmob::ss_get_env_as("cog-shallow-specialize");

	// Fetch data trees
	HandleSeq db_seq = MinerUtils::get_db(db);

	// Get minimum support and maximum number of variables
	unsigned ms = MinerUtils::get_uint(ms_h);
	unsigned mv = MinerUtils::get_uint(mv_h);

	// Generate all shallow specializations
	HandleSet shaspes =
			MinerUtils::shallow_specialize(pattern, db_seq, ms, mv,
					enable_type->getTruthValue()->get_mean() > 0,
					enable_glob->getTruthValue()->get_mean() > 0,
					ignore_vars->getOutgoingSet());

	return asp->add_link(SET_LINK, HandleSeq(shaspes.begin(), shaspes.end()));
}

bool MinerSCM::do_enough_support(Handle pattern, Handle db, Handle ms_h)
{
	// Fetch data trees
	HandleSeq db_seq = MinerUtils::get_db(db);

	// Fetch the minimum support
	unsigned ms = MinerUtils::get_uint(ms_h);

	return MinerUtils::enough_support(pattern, db_seq, ms);
}

Handle MinerSCM::do_expand_conjunction(Handle cnjtion, Handle pattern,
                                       Handle db, Handle ms_h, Handle mv_h,
                                       bool es)
{
	AtomSpacePtr asp = SchemeSmob::ss_get_env_as("cog-expand-conjunction");

	// Fetch data trees
	HandleSeq db_seq = MinerUtils::get_db(db);

	// Get minimum support and maximum variables
	unsigned ms = MinerUtils::get_uint(ms_h);
	unsigned mv = MinerUtils::get_uint(mv_h);

	HandleSet results = MinerUtils::expand_conjunction(cnjtion, pattern,
	                                                   db_seq, ms, mv, es);
	return asp->add_link(SET_LINK, HandleSeq(results.begin(), results.end()));
}

double MinerSCM::do_isurp_old(Handle pattern, Handle db, Handle /*db_ratio*/)
{
	// Fetch data trees
	HandleSeq db_seq = MinerUtils::get_db(db);

	return Surprisingness::isurp_old(pattern, db_seq, false);
}

double MinerSCM::do_nisurp_old(Handle pattern, Handle db, Handle /*db_ratio*/)
{
	// Fetch arguments
	HandleSeq db_seq = MinerUtils::get_db(db);

	return Surprisingness::isurp_old(pattern, db_seq, true);
}

double MinerSCM::do_isurp(Handle pattern, Handle db, Handle db_ratio)
{
	// Fetch arguments
	HandleSeq db_seq = MinerUtils::get_db(db);
	double db_rat = MinerUtils::get_double(db_ratio);

	return Surprisingness::isurp(pattern, db_seq, false, db_rat);
}

double MinerSCM::do_nisurp(Handle pattern, Handle db, Handle db_ratio)
{
	// Fetch arguments
	HandleSeq db_seq = MinerUtils::get_db(db);
	double db_rat = MinerUtils::get_double(db_ratio);

	return Surprisingness::isurp(pattern, db_seq, true, db_rat);
}

TruthValuePtr MinerSCM::do_emp_tv(Handle pattern, Handle db, Handle db_ratio)
{
	// Fetch arguments
	HandleSeq db_seq = MinerUtils::get_db(db);
	double db_rat = MinerUtils::get_double(db_ratio);

	// Calculate its estimate first to optimize empirical calculation
	TruthValuePtr jte = Surprisingness::ji_tv_est_mem(pattern, db_seq);
	return Surprisingness::emp_tv_pbs_mem(pattern, db_seq, jte->get_mean(), db_rat);
}

TruthValuePtr MinerSCM::do_ji_tv_est(Handle pattern, Handle db)
{
	// Fetch data trees
	HandleSeq db_seq = MinerUtils::get_db(db);

	return Surprisingness::ji_tv_est_mem(pattern, db_seq);
}

double MinerSCM::do_jsd(TruthValuePtr ltv, TruthValuePtr rtv)
{
	return Surprisingness::jsd(ltv, rtv);
}

Logger* MinerSCM::do_miner_logger()
{
	return &miner_logger();
}

extern "C" {
void opencog_miner_init(void);
};

void opencog_miner_init(void)
{
    static MinerSCM miner;
    miner.module_init();
}

#endif // HAVE_GUILE
