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

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/guile/SchemeEval.h>

#include <opencog/miner/HandleTree.h>
#include <opencog/miner/Miner.h>

namespace opencog {

class MinerUTestUtils
{
public:
	/**
	 * Add
	 *
	 * (Concept "db")
	 *
	 * to as.
	 */
	static Handle add_db_cpt(AtomSpace& as);

	/**
	 * Add
	 *
	 * (Predicate "minsup")
	 *
	 * to as.
	 */
	static Handle add_minsup_prd(AtomSpace& as);

	/**
	 * Add
	 *
	 * (Predicate <mode>)
	 *
	 * to as.
	 */
	static Handle add_surp_prd(AtomSpace& as, std::string mode);

	/**
	 * Add
	 *
	 * (Lambda (Variable "$X") (Variable "$X"))
	 *
	 * to as.
	 */
	static Handle add_top(AtomSpace& as);

	/**
	 * Add
	 *
	 * (Evaluation <tv>
	 *   (Predicate "minsup")
	 *   (List
	 *     pattern
	 *     (Concept "db")
	 *     minsup))
	 *
	 * to as.
	 */
	static Handle add_minsup_eval(AtomSpace& as,
	                              const Handle& pattern,
	                              int minsup,
	                              TruthValuePtr tv=TruthValue::DEFAULT_TV());

	/**
	 * Repeat add_minsup_eval over a sequence of patterns.
	 */
	static Handle add_minsup_evals(AtomSpace& as,
	                               const HandleSeq& patterns,
	                               int minsup,
	                               TruthValuePtr tv=TruthValue::DEFAULT_TV());

	/**
	 * Insert
	 *
	 * (Evaluation
	 *   (Predicate <mode>)
	 *   (List pattern (Concept "db")))
	 *
	 * to as.
	 */
	static Handle add_surp_eval(AtomSpace& as,
	                            const std::string& mode,
	                            const Handle& pattern);

	/**
	 * Given
	 *
	 * (Evaluation
	 *   (Predicate "minsup")
	 *   (List
	 *     pattern
	 *     db-cpt
	 *     minsup))
	 *
	 * return pattern.
	 *
	 * Note: also works for surp constructs
	 */
	static Handle get_pattern(const Handle& minsup_eval);
	static HandleSeq get_patterns(const HandleSeq& minsup_evals);

	/**
	 * Add
	 *
	 * (Evaluation
	 *   (GroundedPredicate "scm-eager: absolutely-true")
	 *   h)
	 *
	 * to as.
	 */
	static Handle add_abs_true_eval(AtomSpace& as, const Handle& h);

	/**
	 * Add
	 *
	 * (Lambda
	 *   (VariableList X1 ... Xn)
	 *   (Present X1 ... Xn))
	 *
	 * to as.
	 */
	static Handle add_nconjunct(AtomSpace& as, unsigned n);

	/**
	 * Add
	 *
	 * (VariableNode <prefix-i>)
	 *
	 * to as.
	 */
	static Handle add_variable(AtomSpace& as,
	                           const std::string& prefix,
	                           int i);

	/**
	 * Add
	 *
	 * (VariableNode <prefix-0>)
	 * ...
	 * (VariableNode <prefix-n-1>)
	 *
	 * to as.
	 */
	static HandleSeq add_variables(AtomSpace& as,
	                               const std::string& prefix,
	                               int n);

	/**
	 * Add a query to as, run the URE forward to generate patterns,
	 * then backward to gather the results, and output them.
	 */
	static Handle ure_pm(AtomSpace& as,
	                     SchemeEval& scm,
	                     const Handle& pm_rb,
	                     const AtomSpace& db_as,
	                     int minsup,
	                     int max_iter=-1,
	                     Handle initpat=Handle::UNDEFINED,
	                     bool conjunction_expansion=false,
	                     unsigned max_conjuncts=UINT_MAX,
	                     unsigned max_variables=UINT_MAX,
	                     unsigned max_spcial_conjuncts=1,
	                     unsigned max_cnjexp_variables=UINT_MAX,
	                     bool enforce_specialization=true,
	                     double complexity_penalty=0.0,
	                     bool enable_type=false,
	                     bool enable_glob=false,
	                     std::vector<std::string> ignore_vars={});
	static Handle ure_pm(AtomSpace& as,
	                     SchemeEval& scm,
	                     const Handle& pm_rb,
	                     const HandleSeq& db, int minsup,
	                     int max_iter=-1,
	                     Handle initpat=Handle::UNDEFINED,
	                     bool conjunction_expansion=false,
	                     unsigned max_conjuncts=UINT_MAX,
	                     unsigned max_variables=UINT_MAX,
	                     unsigned max_spcial_conjuncts=1,
	                     unsigned max_cnjexp_variables=UINT_MAX,
	                     bool enforce_specialization=true,
	                     double complexity_penalty=0.0,
	                     bool enable_type=false,
	                     bool enable_glob=false,
	                     std::vector<std::string> ignore_vars={});

	/**
	 * Configure the C++ Miner and run it.
	 */
	static HandleTree cpp_pm(const AtomSpace& db_as,
	                         int minsup=1,
	                         int conjuncts=1,
	                         const Handle& initpat=Handle::UNDEFINED,
	                         int maxdepth=-1);
	static HandleTree cpp_pm(const HandleSeq& db,
	                         int minsup=1,
	                         int conjuncts=1,
	                         const Handle& initpat=Handle::UNDEFINED,
	                         int maxdepth=-1);

	/**
	 * Add
	 *
	 * Lambda
	 *   X
	 *   Inheritance
	 *     X
	 *     cpt
	 *
	 * to as.
	 */
	static Handle add_is_cpt_pattern(AtomSpace& as, const Handle& cpt);

	/**
	 * Add
	 *
	 * Lambda
	 *   X
	 *   Inheritance
	 *     X
	 *     {Concept "ugly", Concept "man", Concept "soda_drinker"}
	 *
	 * to as.
	 */
	static Handle add_ugly_pattern(AtomSpace& as);
	static Handle add_man_pattern(AtomSpace& as);
	static Handle add_soda_drinker_pattern(AtomSpace& as);

	/**
	 * Add
	 *
	 * Lambda
	 *   X
	 *   And
	 *     Inheritance
	 *       X
	 *       Concept "ugly"
	 *     Inheritance
	 *       X
	 *       Concept "man"
	 *
	 * to as.
	 */
	static Handle add_ugly_man_pattern(AtomSpace& as);

	/**
	 * Add the following pattern
	 *
	 * Lambda
	 *   X
	 *   And
	 *     Inheritance
	 *       X
	 *       Concept "man"
	 *     Inheritance
	 *       X
	 *       Concept "soda drinker"
	 *     Inheritance
	 *       X
	 *       Concept "ugly"
	 *
	 * to as.
	 */
	static Handle add_ugly_man_soda_drinker_pattern(AtomSpace& as);

	static void configure_mandatory_rules(SchemeEval& scm);
	static void configure_optional_rules(SchemeEval& scm,
	                                     bool conjunction_expansion,
	                                     unsigned max_conjuncts=UINT_MAX,
	                                     unsigned max_variables=UINT_MAX,
	                                     unsigned max_spcial_conjuncts=1,
	                                     unsigned max_cnjexp_variables=UINT_MAX,
	                                     bool enforce_specialization=false,
	                                     bool enable_type=false,
	                                     bool enable_glob=false,
	                                     std::vector<std::string> ignore_vars={});
	static void configure_surprisingness(SchemeEval& scm,
	                                     const Handle& surp_rb,
	                                     const std::string& mode,
	                                     unsigned max_conjuncts,
	                                     double db_ratio);

	/**
	 * Run I-Surprisingness reasoning on the current atomspace and
	 * return the result sorted by I-Surprisingness.
	 */
	static HandleSeq ure_surp(AtomSpace& as,
	                          SchemeEval& scm,
	                          const Handle& surp_rb,
	                          const std::string& mode,
	                          unsigned max_conjuncts,
	                          double db_ratio=1.0);

	/**
	 * Populate the given atomspace with n nodes of a given type, named
	 * prefix + std::to_string(i) with i in [0, n).
	 *
	 * The handles of the populated atoms.
	 */
	static HandleSeq populate_nodes(AtomSpace& as,
	                                unsigned n,
	                                Type type,
	                                const std::string& prefix);

	/**
	 * Populate the given atomspace by creating links of a given type
	 * and arity with probability p between any arity handles from
	 * hs. Return the handles of the populated atoms.
	 */
	static HandleSeq populate_links(AtomSpace& as,
	                                const HandleSeq& hs,
	                                Type type,
	                                unsigned arity,
	                                double p);

	/**
	 * Add a default variable declaration if pattern is missing one.
	 */
	static Handle add_default_vardecl(const Handle& pattern);
};

} // ~namespace opencog
