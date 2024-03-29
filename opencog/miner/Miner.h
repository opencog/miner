/*
 * Miner.h
 *
 * Copyright (C) 2017 OpenCog Foundation
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
#ifndef OPENCOG_MINER_H_
#define OPENCOG_MINER_H_

#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/core/Variables.h>
#include <opencog/atoms/core/RewriteLink.h>
#include <opencog/atomspace/AtomSpace.h>

#include "HandleTree.h"
#include "Valuations.h"
#include "MinerUtils.h"

class MinerUTest;

namespace opencog
{

/**
 * Parameters for Miner. The terminology is taken from
 * Frequent Subtree Mining -- An Overview, from Yun Chi et al, when
 * possible.
 */
struct MinerParameters {

	/**
	 * CTor. Note that conjuncts will be overwritten by initpat, if
	 * provided.
	 */
	MinerParameters(unsigned minsup=1,
	                unsigned conjuncts=1,
	                const Handle& initpat=Handle::UNDEFINED,
	                int maxdepth=-1);

	// TODO: change frequency by support!!!
	// Minimum support. Mined patterns must have a frequency equal or
	// above this value.
	unsigned minsup;

	// Initial number of conjuncts. This value is overwritten by the
	// actual number of conjuncts of initpat, if provided.
	unsigned initconjuncts;

	// Initial pattern. All found patterns are specialization of
	// it. If UNDEFINED, then the initial pattern is the most abstract
	// one. i.e.
	//
	// Lambda
	//   X
	//   X
	//
	// That is the pattern matching the entire atomspace.
	Handle initpat;

	// Maximum depth from pattern to output. If negative, then no
	// depth limit. Depth is the number of specializations between the
	// initial pattern and the produced patterns.
	int maxdepth;
};

/**
 * Experimental pattern miner. Mined patterns should be compatible
 * with the pattern matcher, that is if feed to the pattern matcher,
 * the latter should return as many candidates as the pattern's
 * frequency.
 */
class Miner
{
    friend class ::MinerUTest;
public:

	/**
	 * CTor
	 */
	Miner(const MinerParameters& param=MinerParameters());

	/**
	 * Mine the given AtomSpace and return a tree of patterns linked by
	 * specialization relationship (children are specializations of
	 * parent) with frequency equal to or above minsup, starting from
	 * the initial pattern, excluded.
	 */
	HandleTree operator()(const AtomSpace& db_as);

	/**
	 * Like above but only mine amongst the provided data tree collection.
	 */
	HandleTree operator()(const HandleSeq& db);

	/**
	 * Specialization. Given a pattern and a collection of data trees,
	 * generate all specialized patterns of the given pattern.
	 */
	HandleTree specialize(const Handle& pattern,
	                      const HandleSeq& db,
	                      int maxdepth=-1);

	/**
	 * Like above, where all valid data trees have been converted into
	 * valuations.
	 */
	HandleTree specialize(const Handle& pattern,
	                      const HandleSeq& db,
	                      const Valuations& valuations,
	                      int maxdepth);

	/**
	 * Alternate specialization that reflects how the URE would work.
	 */
	HandleTree specialize_alt(const Handle& pattern,
	                          const HandleSeq& db,
	                          const Valuations& valuations,
	                          int maxdepth);

	// Parameters
	MinerParameters param;

private:

	mutable AtomSpacePtr tmp_as;

	/**
	 * Return true iff maxdepth is null or pattern is not a lambda or
	 * doesn't have enough support. Additionally the second one check
	 * whether the valuation has any variable left to specialize from.
	 */
	bool terminate(const Handle& pattern,
	               const HandleSeq& db,
	               const Valuations& valuations,
	               int maxdepth) const;

	/**
	 * Specialize the given pattern according to shallow abstractions
	 * obtained by looking at the valuations of the front variable of
	 * valuations, then recursively call Miner::specialize on these
	 * obtained specializations.
	 */
	HandleTree specialize_shabs(const Handle& pattern,
	                            const HandleSeq& db,
	                            const Valuations& valuations,
	                            int maxdepth);

	/**
	 * Specialize the given pattern with the given shallow abstraction
	 * at the given variable, then call Miner::specialize on the
	 * obtained specialization.
	 */
	HandleTree specialize_shapat(const Handle& pattern,
	                             const HandleSeq& db,
	                             const Handle& var,
	                             const Handle& shapat,
	                             int maxdepth);

	/**
	 * Calculate if the pattern has enough support w.r.t. to the given
	 * db, that is whether its frequency is greater than or equal
	 * to minsup.
	 */
	bool enough_support(const Handle& pattern,
	                    const HandleSeq& db) const;

	/**
	 * Given a pattern and a db, calculate the pattern support, that is
	 * the number of matches if pattern is strongly connected.
	 *
	 * If pattern is not strongly connected AND some heuristic is in
	 * place TODO, then the definition of frequency deviates from the
	 * usual one and corresponds to the minimum frequency over all
	 * strongly connected components of that pattern.
	 *
	 * ms is used to halt the frequency calculation if it reaches a
	 * certain maximum, for saving resources.
	 */
	unsigned support(const Handle& pattern,
	                 const HandleSeq& db,
	                 unsigned ms) const;

	/**
	 * Calculate the frequency of the whole pattern, given the
	 * frequency of it's components.
	 */
	unsigned freq(const std::vector<unsigned>& freqs) const;

	/**
	 * Filter in only db matching the pattern
	 */
	HandleSeq filter_db(const Handle& pattern,
	                       const HandleSeq& db) const;

	/**
	 * Check whether a pattern matches a dt.
	 */
	bool match(const Handle& pattern, const Handle& dt) const;

	/**
	 * Like above but returns the Set of Lists of values associated to
	 * the variables of the pattern. Assumes that pattern is always a
	 * LambdaLink, and not a constant.
	 *
	 * TODO: optimize
	 */
	Handle matched_results(const Handle& pattern, const Handle& dt) const;
};

} // ~namespace opencog

#endif /* OPENCOG_MINER_H_ */
