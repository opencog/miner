/*
 * MinerUtils.h
 *
 * Copyright (C) 2018 SingularityNET Foundation
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
#ifndef OPENCOG_MINER_UTILS_H_
#define OPENCOG_MINER_UTILS_H_

#include <opencog/util/empty_string.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/unify/Unify.h>

#include "Valuations.h"

namespace opencog
{

typedef std::vector<HandleSeqSeq> HandleSeqSeqSeq;
typedef std::pair<HandleSet, GlobInterval> ValIntvlPair;
typedef std::map<Handle, ValIntvlPair> HandleValIntvlMap;

/**
 * Collection of static methods for the pattern miner.
 */
class MinerUtils
{
public:
	/**
	 * Wrap conjuncts (including unary) with PresentLink rather than
	 * AndLink.
	 */
	static const bool use_present_link = true;

	/**
	 * Given valuations produce all shallow abstractions reaching
	 * minimum support, over all variables. It basically applies
	 * focus_shallow_abstract recursively. See the specification of
	 * focus_shallow_abstract for more information.
	 *
	 * For instance, given
	 *
	 * valuations =
	 *   { { X->(Inheritance (Concept "A") (Concept "B")), Y->(Concept "C") },
	 *     { X->(Inheritance (Concept "B") (Concept "C")), Y->(Concept "D") },
	 *     { X->(Concept "E"), Y->(Concept "D") } }
	 * ms = 2
	 *
	 * shallow_abstract(valuations) =
	 *  {
	 *    ;; Shallow abstractions of X
	 *    { (Lambda
	 *        (VariableSet
	 *          (Variable "$X1")
	 *          (Variable "$X2"))
	 *        (Inheritance
	 *          (Variable "$X1")
	 *          (Variable "$X2"))) },
	 *    ;; Shallow abstractions of Y
	 *    { (Concept "D") }
	 *  }
	 */
	static HandleSetSeq shallow_abstract(const Valuations &valuations,
	                                     unsigned ms,
	                                     bool enable_type,
	                                     bool enable_glob,
	                                     const HandleSeq& ignore_vars);

	/**
	 * Given valuations produce all shallow abstractions reaching
	 * minimum support based on the values associated to the variable
	 * in focus. This shallow abstractions include
	 *
	 * 1. Single operator patterns, like (Lambda X Y (Inheritance X Y))
	 * 2. Constant nodes, like (Concept "A")
	 * 3. Remain variable after the front one, x2, ..., xn
	 *
	 * Composing these 3 sorts of abstractions are enough to generate
	 * all possible patterns.
	 *
	 * For instance, given
	 *
	 * valuations =
	 *   { { X->(Inheritance (Concept "A") (Concept "B")), Y->(Concept "C") },
	 *     { X->(Inheritance (Concept "B") (Concept "C")), Y->(Concept "D") },
	 *     { X->(Concept "E"), Y->(Concept "D") } }
	 * ms = 2
	 *
	 * front_shallow_abstract(valuations) = { (Lambda
	 *                                          (VariableSet
	 *                                            (Variable "$X1")
	 *                                            (Variable "$X2"))
	 *                                          (Inheritance
	 *                                            (Variable "$X1")
	 *                                            (Variable "$X2"))) }
	 */
	static HandleSet focus_shallow_abstract(const Valuations &valuations,
	                                        unsigned ms, bool enable_type,
	                                        bool enable_glob);

	/**
	 * Return true iff h is a node or a nullary link.
	 */
	static bool is_nullary(const Handle& h);

	/**
	 * Given an atom, a value, return its corresponding shallow
	 * abstraction. A shallow abstraction of an atom is
	 *
	 * 1. itself if it is nullary (see is_nullary)
	 *
	 * 2. (Lambda (VariableSet X1 ... Xn) (L X1 ... Xn) if it is a
	 *    link of arity n.
	 *
	 * For instance, with
	 *
	 * dt = (Inheritance (Concept "a") (Concept "b"))
	 *
	 * shallow_patterns(dt) = (Lambda
	 *                          (VariableSet
	 *                            (Variable "$X1")
	 *                            (Variable "$X2"))
	 *                          (Inheritance
	 *                            (Variable "$X1")
	 *                            (Variable "$X2")))
	 *
	 * TODO: we may want to support types in variable declaration.
	 */
	static Handle shallow_abstract_of_val(const Handle& value);

	static Handle shallow_abstract_of_val(const Handle& value, const HandleSeq& rnd_vars);

	static HandleSeq glob_shallow_abstract_of_val(const Handle &val,
	                                              const Handle &var, bool enable_type);

	static HandleSeq glob_shallow_abstract_of_lst(const Handle &value,
	                                              const HandleSeq &vars, bool enable_type);

	/**
	 * Wrap a VariableSet around a list of variables if more than one
	 * variable, otherwise return that one variable.
	 */
	static Handle variable_set(const HandleSeq& vars);

	/**
	 * Wrap a LambdaLink around a vardecl and body.
	 */
	static Handle lambda(const Handle& vardecl, const Handle& body);

	/**
	 * Wrap a QuoteLink around h
	 */
	static Handle quote(const Handle& h);

	/**
	 * Wrap a UnquoteLink around h
	 */
	static Handle unquote(const Handle& h);

	/**
	 * Wrap a LocalQuote link around h (typically used if it is a link
	 * of type AndLink. That is in order not to produce multi-conjuncts
	 * patterns when in fact we want to match an AndLink data tree.)
	 */
	static Handle local_quote(const Handle& h);

	/**
	 * Given a pattern, and mapping from variables to sub-patterns,
	 * compose (as in function composition) the pattern with the
	 * sub-patterns. That is replace variables in the pattern by their
	 * associated sub-patterns, properly updating the variable
	 * declaration.
	 */
	static Handle compose(const Handle& pattern, const HandleMap& var2pat);

	/**
	 * It does the same as compose except when the value to be substituted
	 * is a Variable, beta_reduction wont pass type checking. This is an
	 * alternative using replace_nocheck.
	 */
	static Handle compose_nocheck(const Handle& pattern, const HandlePair& var2pat);

	/**
	 * Given a db concept node, retrieve all its members
	 */
	static HandleSeq get_db(const Handle& db_cpt);

	/**
	 * Return the non-negative integer held by a number node.
	 */
	static unsigned get_uint(const Handle& h);

	/**
	 * Return the double held by a number node.
	 */
	static double get_double(const Handle& h);

	/**
	 * Given a pattern and a db, calculate the pattern frequency up to
	 * ms (to avoid unnecessary calculations).
	 */
	static unsigned support(const Handle& pattern,
	                        const HandleSeq& db,
	                        unsigned ms);

	/**
	 * Like support but assumes that pattern is strongly connected (all
	 * its variables depends on other clauses).
	 */
	static unsigned component_support(const Handle& pattern,
	                                  const HandleSeq& db,
	                                  unsigned ms);

	/**
	 * Calculate if the pattern has enough support w.r.t. to the given
	 * db, that is whether its frequency is greater than or equal
	 * to ms.
	 */
	static bool enough_support(const Handle& pattern,
	                           const HandleSeq& db,
	                           unsigned ms);

	/**
	 * Like shallow_abstract(const Valuations&, unsigned) but takes a pattern
	 * and a db instead, and generate the valuations of the pattern
	 * prior to calling shallow_abstract on its valuations.
	 *
	 * See comment on shallow_abstract(const Valuations&, unsigned) for more
	 * details.
	 */
	static HandleSetSeq shallow_abstract(const Handle& pattern,
	                                     const HandleSeq& db,
	                                     unsigned ms,
	                                     bool enable_type,
	                                     bool enable_glob,
	                                     const HandleSeq& ignore_vars);

	/**
	 * Return all shallow specializations of pattern with support ms
	 * according to db.
	 *
	 * mv is the maximum number of variables allowed in the resulting
	 * patterns.
	 *
	 * enable_type is a flag controlling whether type declaration is
	 * specialized as well.  For instance if a given variable is only
	 * matching concept nodes, then if enable_type is set to true, such
	 * variable will be type restricted to concept nodes in the
	 * specialized pattern.
	 *
	 * enable_glob is a flag controlling whether GlobNode may be
	 * supported in the specialized patterns.  Convenient for matching
	 * links with varying arity.
	 *
	 * ignore_vars is a set of variables not to specialize.  This is
	 * convenient for instance for temporal mining, where the temporal
	 * variable must not be specialized.
	 */
	static HandleSet shallow_specialize(const Handle& pattern,
	                                    const HandleSeq& db,
	                                    unsigned ms,
	                                    unsigned mv=UINT_MAX,
	                                    bool enable_type=false,
	                                    bool enable_glob=false,
	                                    const HandleSeq& ignore_vars={});

	/**
	 * Create a pattern body from clauses, introducing an AndLink if
	 * necessary.
	 */
	static Handle mk_body(const HandleSeq clauses);

	/**
	 * Given a sequence of clause create a LambdaLink of it without
	 * variable declaration, introducing a AndLink or PresentLink if
	 * necessary.
	 */
	static Handle mk_pattern_no_vardecl(const HandleSeq& clauses);

	/**
	 * Given a vardecl and a sequence of clauses, filter the vardecl to
	 * contain only variable of the body, and create a Lambda with
	 * them.
	 */
	static Handle mk_pattern_filtering_vardecl(const Handle& vardecl,
	                                           const HandleSeq& clauses);

	/**
	 * Given a vardecl and a sequence of clauses, build a pattern. If
	 * use_present_link is true, then the result will be
	 *
	 * (Lambda <vardecl> (Present <clauses-1> ... <clauses-n>))
	 */
	static Handle mk_pattern(const Handle& vardecl, const HandleSeq& clauses);

	/**
	 * Given a pattern, split it into smaller patterns of strongly
	 * connected components.
	 */
	static HandleSeq get_component_patterns(const Handle& pattern);

	/**
	 * Like above but consider a sequence of clauses instead of a
	 * pattern, and return a sequence of sequences of clauses.
	 */
	static HandleSeqSeq get_components(const HandleSeq& clauses);

	/**
	 * Given a pattern, split it into its disjuncts.
	 */
	static HandleSeq get_conjuncts(const Handle& pattern);

	/**
	 * Given a pattern and db, return the satisfying set of the pattern
	 * over the data tree.
	 *
	 * TODO: ignore permutations for unordered links.
	 *
	 * TODO: ignore duplicates within the same data tree. For instance
	 * if the pattern is
	 *
	 * (Lambda (LocalQuote (And (Variable "$X") (Variable "$Y"))))
	 *
	 * and the db is
	 *
	 * { (And (Concept "A") (And (Concept "B") (Concept "C"))) }
	 *
	 * then the result will include 2 results
	 *
	 * { (And (Concept "A") (And (Concept "B") (Concept "C"))),
	 *   (And (Concept "B") (Concept "C")) }
	 *
	 * instead of one
	 *
	 * { (And (Concept "A") (And (Concept "B") (Concept "C"))) }
	 *
	 * Also, the pattern may match any subhypergraph of db, not just
	 * the root atoms (TODO: we probably don't want that!!!).
	 */
	static Handle restricted_satisfying_set(const Handle& pattern,
	                                        const HandleSeq& db,
	                                        unsigned ms=UINT_MAX);

	/**
	 * Return true iff the pattern is totally abstract like
	 *
	 * (Lambda
	 *   (Variable "$X")
	 *   (Variable "$X"))
	 *
	 * for a single conjunct. Or
	 *
	 * (Lambda
	 *   (List
	 *     (Variable "$X")
	 *     (Variable "$Y"))
	 *   (And
	 *     (Variable "$X")
	 *     (Variable "$Y"))
	 *
	 * for 2 conjuncts, etc.
	 */
	static bool totally_abstract(const Handle& pattern);

	/**
	 * Generate a list of hopefully unique random variables
	 */
	static HandleSeq gen_rand_variables(size_t n);
	static Handle gen_rand_variable();

	static HandleSeq gen_rand_globs(size_t n);
	static Handle gen_rand_glob();

	/**
	 * Given a pattern return its variables. If the pattern is not a
	 * scope link (i.e. a data tree), then return the empty Variables.
	 */
	static const Variables& get_variables(const Handle& pattern);

	/**
	 * Given a pattern, return its vardecl. If the pattern is not a
	 * scope link (i.e. a data tree), then return the empty vardecl.
	 */
	static Handle get_vardecl(const Handle& pattern);

	/**
	 * Given a pattern, return its body. If the pattern is not a scope
	 * link (i.e. a data tree), then return pattern itself.
	 */
	static const Handle& get_body(const Handle& pattern);

	/**
	 * Given a pattern, return its clause. If the pattern is not a
	 * scope link (i.e. a data tree), then behavior is undefined.
	 */
	static HandleSeq get_clauses(const Handle& pattern);
	static HandleSeq get_clauses_of_body(const Handle& body);

	/**
	 * Return the number of conjuncts in a pattern. That is, if the
	 * pattern body is an AndLink, then returns its arity, otherwise
	 * if the body is not an AndLink, then return 1, and if it's not a
	 * pattern at all (i.e. not a LambdaLink), then return 0.
	 */
	static unsigned n_conjuncts(const Handle& pattern);

	/**
	 * Remove useless clauses from a body pattern. Useless clauses are
	 * constant clauses, as well as variables that already occur
	 * within an existing clause.
	 */
	static Handle remove_useless_clauses(const Handle& pattern);
	static Handle remove_useless_clauses(const Handle& vardecl,
	                                     const Handle& body);
	static void remove_useless_clauses(const Handle& vardecl,
	                                   HandleSeq& clauses);

	/**
	 * Remove any closes clause (regardless of whether they are
	 * evaluatable or not).
	 */
	static void remove_constant_clauses(const Handle& vardecl,
	                                    HandleSeq& clauses);

	/**
	 * Remove redundant subclauses, such as ones identical to clauses
	 * of there subtrees.
	 */
	static void remove_redundant_subclauses(HandleSeq& clauses);

	/**
	 * Remove redundant clauses.
	 */
	static void remove_redundant_clauses(HandleSeq& clauses);

	/**
	 * Remove clauses that are more abstract than all other clauses,
	 * thus should not change the semantics of the pattern.
	 *
	 * For instance given the clauses
	 *
	 *     (Inheritance (Variable "$X") (Concept "pet"))
	 *     (Inheritance (Concept "cat") (Variable "$Y"))
	 *     (Inheritance (Variable "$X") (Variable "$Y")))
	 *
	 * the last one is more abstract than either of the first 2, thus
	 * can be removed.
	 */
	static void remove_abstract_clauses(HandleSeq& clauses);

	/**
	 * Return true iff clause includes only joint variables.
	 *
	 * For instance given
	 *
	 * clause = (Inheritance (Variable "$X") (Variable "$Y"))
	 *
	 * clauses = { (Inheritance (Variable "$X") (Concept "pet"))
	 *             (Inheritance (Concept "cat") (Variable "$Y")) }
	 *
	 * return true because all variables of clause are joint with
	 * clauses.
	 */
	static bool has_only_joint_variables(const Handle& clause,
	                                     const HandleSeq& clauses);

	/**
	 * Tell whether the left block/subpattern is is syntactically more
	 * abstract than the right block/subpattern relative to a given
	 * variable.
	 *
	 * For instance
	 *
	 * l_blk = { List X Y Z }
	 * r_blk = { List W A Z }
	 *
	 * l_blk is more abstract than r_blk relative to Z because the
	 * matching values of Z in r_blk is a subset of the matching values
	 * of Z in l_blk.
	 *
	 * Warning to future developers: this method and the one below have
	 * different names (that one uses `blk` while the one below uses
	 * `pat` because gcc is not able to disambiguating them. For
	 * instance in is_pat_syntax_more_abstract({lp}, {rp}, var) gcc
	 * will not understand that {lp} and {rp} are meant as being
	 * HandleSeqs. Not sure why that is the case, maybe because {} is
	 * not exclusive to intializer_list.
	 */
	static bool is_blk_syntax_more_abstract(const HandleSeq& l_blk,
	                                        const HandleSeq& r_blk,
	                                        const Handle& var);

	/**
	 * Like above but takes scope links instead of blocks (whether each
	 * scope link has the conjunction of clauses of its block as body).
	 *
	 * TODO: for now, this code relies on unification. However it can
	 * certainly be optimized by not relying on unification and being
	 * re-implemented instead, and perhaps it could then be merged to
	 * the unification code.
	 */
	static bool is_pat_syntax_more_abstract(const Handle& l_pat,
	                                        const Handle& r_pat,
	                                        const Handle& var);

	/**
	 * Like is_syntax_more_abstract but takes into account a bit of
	 * semantics as well (though none that requires data), in
	 * particular it handles conjunctions of clauses such that l_pat is
	 * more abstract if there exists a partition lp of l_pat (meaning a
	 * partition of conjunctions of clauses in l_pat) such that there
	 * exists a subset rs of r_pat (meaning a subset of clauses of
	 * r_pat) such that rs is a syntactic specialization, relative to
	 * var, of each block lb of lp.  This indeed garanties that l_pat
	 * is more abstract than r_pat, find the proof below.
	 *
	 * Let lp be a partition of l_blk and lbᵢ be the blocks of lp, let
	 * rs be a subset of r_blk.  Let mᵢ be the matching values of var
	 * according to pattern bᵢ and ms be the matching values of var
	 * according to pattern rs.
	 *
	 * ∀ i ranging over the blocks of lp, if lbᵢ is syntactically more
	 * abstract than rs, then ms ⊆ mᵢ.  Therefore ms ⊆ (⋂ᵢ mᵢ),
	 * therefore l_blk is more abstract than rs.  Since rs is a subset
	 * (i.e. a sub-conjunction) of r_blk, it is necessarily more
	 * abstract than r_blk.  Since l_blk is more abstract than rs and
	 * rs is more abstract than r_blk, by transitivity, l_blk is more
	 * abstract than r_blk.  Quod Erat Demonstrandum.
	 *
	 * For instance
	 *
	 * l_pat = Lambda
	 *           X Y Z
	 *           And
	 *             Inheritance
	 *               X
	 *               Z
	 *             Inheritance
	 *               Y
	 *               Z
	 *
	 * r_pat = Lambda
	 *           Z
	 *           Inheritance
	 *             A
	 *             Z
	 *
	 * var = Z
	 *
	 * l_pat is indeed an abstraction of r_pat because there exists a
	 * partition lp = { {Inheritance X Z}, {Inheritance Y Z} } such
	 * that the subset { Inheritance A Z} is a syntactic specialization
	 * of each block of lp.
	 */
	static bool is_pat_more_abstract(const Handle& l_pat,
	                                 const Handle& r_pat,
	                                 const Handle& var);

	/**
	 * Like above but consider list of clauses (i.e. block) instead of
	 * patterns.
	 */
	static bool is_blk_more_abstract(const HandleSeq& l_blk,
	                                 const HandleSeq& r_blk,
	                                 const Handle& var);

	/**
	 * Return true iff for each variable v in clause, let o(v) be all
	 * clauses containing v, clause is more abstract than any clauses
	 * in o(v) relative to v.
	 */
	static bool is_more_abstract_foreach_var(const Handle& clause,
	                                         const HandleSeq& others);

	/**
	 * Like powerset but return a sequence of sequences instead of set
	 * of sets. Discard the empty sequence.
	 */
	static HandleSeqSeq powerseq_without_empty(const HandleSeq& blk);


	/**
	 * Alpha convert pattern so that none of its variables collide with
	 * the variables in other_vars.
	 */
	static Handle alpha_convert(const Handle& pattern,
	                            const Variables& other_vars);

	/**
	 * Return true iff var_val is a pair with the first element a
	 * variable in vars, and the second element a value (non-variable).
	 */
	static bool is_value(const Unify::HandleCHandleMap::value_type& var_val,
	                     const Variables& vars, const Handle& var);

	/**
	 * Copy all subpatterns/blocks where var appears. Also remove all
	 * parts of the subpatterns that are not strongly connected with to
	 * it relative to var.
	 *
	 * So for instance
	 *
	 * partition = { { Inheritance X Y, Inheritance Z A},
	 *               { Inheritance X B, Inheritance Z Y} }
	 *
	 * var = Y
	 *
	 * returns
	 *
	 * { {Inheritance Z Y } }
	 *
	 * because
	 *
	 * 1. Y only appears in the second block
	 *
	 * 2. within that block
	 *
	 *    Inheritance X B
	 *
	 *    is not strongly connected to the component where Y appears.
	 *
	 * Ignoring non-strongly connected components allows to speed up
	 * Surprisingness::value_count as well as covering more cases in
	 * is_more_abstract.
	 */
	static HandleSeqSeq connected_subpatterns_with_var(const HandleSeqSeq& partition,
	                                                   const Handle& var);
	static HandleSeq connected_subpattern_with_var(const HandleSeq& blk,
	                                               const Handle& var);

	/**
	 * Given a handle h and a sequence of sequences of handles, insert
	 * h in front of each subsequence, duplicating each sequence with
	 * its augmented subsequence. For instance
	 *
	 * h = D
	 * hss = [[A],[B,C]]
	 *
	 * returns
	 *
	 * [[[D,A],[B,C]],[[A],[D,B,C]],[[A],[B,C],[D]]]
	 */
	static HandleSeqSeqSeq combinatorial_insert(const Handle& h,
	                                            const HandleSeqSeq& hss);
	static HandleSeqSeqSeq combinatorial_insert(const Handle& h,
	                                            HandleSeqSeq::const_iterator from,
	                                            HandleSeqSeq::const_iterator to);

	/**
	 * Given a HandleSeq hs, produce all partitions of hs. For instance
	 * if hs is the following
	 *
	 * c = [A,B,C]
	 *
	 * return
	 *
	 * [[[A],[C],[B]],
	 *  [[C,A],[B]],
	 *  [[C],[B,A]],
	 *  [[A],[C,B]],
	 *  [[C,B,A]]]
	 */
	static HandleSeqSeqSeq partitions(const HandleSeq& hs);
	static HandleSeqSeqSeq partitions(HandleSeq::const_iterator from,
	                                  HandleSeq::const_iterator to);

	/**
	 * Like partitions but takes a pattern. Also the partition block
	 * corresponding to the full set has been removed (since it is
	 * already the block corresponding to the full pattern). For
	 * instance
	 *
	 * pattern = Lambda
	 *             And
	 *               A
	 *               B
	 *               C
	 *
	 * return
	 *
	 * [[[A],[C],[B]],
	 *  [[C,A],[B]],
	 *  [[C],[B,A]],
	 *  [[A],[C,B]]]
	 */
	static HandleSeqSeqSeq partitions_without_pattern(const Handle& pattern);

	/**
	 * Construct the conjunction of 2 patterns. If cnjtion is a
	 * conjunction, then expand it with pattern (performing
	 * alpha-conversion when necessary). It is assumed that pattern
	 * cannot be a conjunction itself.
	 *
	 * This method will not attempt to connect the 2 patterns, thus,
	 * assuming that cnjtion is itself strongly connected, the result
	 * will be 2 strongly connected components.
	 */
	static Handle expand_conjunction_disconnect(const Handle& cnjtion,
	                                            const Handle& pattern);

	/**
	 * Like expand_conjunction_disconnect but produced a single
	 * strongly connected component, assuming that cnjtion is itself
	 * strongly connected, given 2 connecting variables, one from
	 * cnjtion, one from pattern.
	 *
	 * Unlike expand_conjunction_disconnect, no alpha conversion is
	 * performed, cnjtion is assumed not to collide with pattern.
	 */
	static Handle expand_conjunction_connect(const Handle& cnjtion,
	                                         const Handle& pattern,
	                                         const Handle& cnjtion_var,
	                                         const Handle& pattern_var);

	/**
	 * Like expand_conjunction_connect but consider a mapping from
	 * variables of pattern to variables of cnjtion.
	 */
	static Handle expand_conjunction_connect(const Handle& cnjtion,
	                                         const Handle& pattern,
	                                         const HandleMap& pv2cv);

	/**
	 * Like expand_conjunction_connect but recursively consider all
	 * variable mappings from pattern to cnjtion.
	 *
	 * pvi is the variable index of pattern variable declaration.
	 */
	static HandleSet expand_conjunction_rec(const Handle& cnjtion,
	                                        const Handle& pattern,
	                                        const HandleSeq& db,
	                                        unsigned ms,
	                                        unsigned mv,
	                                        const HandleMap& pv2cv=HandleMap(),
	                                        unsigned pvi=0);

	/**
	 * Like expand_conjunction_rec but enforce specialization. Mean
	 * only total mappings from the variables of pattern to the
	 * variables of cnjtion will be considered, as to not introduced
	 * any new variables.
	 */
	static HandleSet expand_conjunction_es_rec(const Handle& cnjtion,
	                                           const Handle& pattern,
	                                           const HandleSeq& db,
	                                           unsigned ms,
	                                           unsigned mv,
	                                           const HandleMap& pv2cv=HandleMap(),
	                                           unsigned pvi=0);

	/**
	 * Given cnjtion and pattern, consider all possible connections
	 * (a.k.a linkages) and expand cnjtion accordingly. For instance if
	 *
	 * cnjtion = (Inheritance X Y)
	 * pattern = (Inheritance Z W)
	 *
	 * return
	 *
	 *   (And (Inheritance X Y) (Inheritance X W))
	 *   (And (Inheritance X Y) (Inheritance Z X))
	 *   (And (Inheritance X Y) (Inheritance Y W))
	 *   (And (Inheritance X Y) (Inheritance X Y))
	 *
	 * It will also only include patterns with minimum support ms
	 * according to db, and perform alpha-conversion when necessary.
	 * If an expansion is cnjtion itself it will be dismissed.
	 *
	 * mv is the maximum number of variables allowed in the resulting
	 *    patterns.
	 *
	 * es is a flag to enforce specialization by
	 *    discarding new variables.
	 */
	static HandleSet expand_conjunction(const Handle& cnjtion,
	                                    const Handle& pattern,
	                                    const HandleSeq& db,
	                                    unsigned ms,
	                                    unsigned mv=UINT_MAX,
	                                    bool es=true);

	/**
	 * Return an atom to serve as key to store the support value.
	 */
	static const Handle& support_key();

	/**
	 * Attach the support of a pattern to support_key(). The support is
	 * encoded as double because it is stored as a FloatValue, and its
	 * subsequent processing (probability estimate, etc) requires a
	 * double anyway.
	 */
	static void set_support(const Handle& pattern, double support);

	/**
	 * Get the support of a pattern stored as associated value to
	 * support_key(). If no such value exist then return -1.0.
	 */
	static double get_support(const Handle& pattern);

	/**
	 * Like get_support, but if there is no value associated to
	 * support_key() then calculate and set the support.
	 *
	 * Warning: note that the support is gonna be up to ms, so such
	 * memoization should not be used if ms is to be changed.
	 */
	static double support_mem(const Handle& pattern,
	                          const HandleSeq& db,
	                          unsigned ms);

	/**
	 * Remove every element of clauses such that
	 *
	 * fun(element, clauses - {element})
	 *
	 * returns true.
	 */
	static void remove_if(HandleSeq& clauses,
	                      std::function<bool(const Handle&, const HandleSeq&)> fun);

	static HandleSet type_restrict_patterns(const HandleSeqMap &);

	static Handle type_restrict_pattern(const HandleSeqMap::value_type &pair);

	static Handle lwst_com_types_decl(const Handle &var, const HandleSeq &vector,
	                                  const GlobInterval &);

	static TypeSet lwst_com_types(HandleSeq vals);

	static TypeSet lwst_com_types(TypeSet tsets);

	static HandleValIntvlMap simple_unify(const HandleSeq &pat, const HandleSeq &val);

	static void extend_seq_map(HandleValIntvlMap &sup, const HandleValIntvlMap &sub);
};

/**
 * Given a partition, that is a sequence of blocks, where each
 * block is a sequence of handles, return
 */
std::string oc_to_string(const HandleSeqSeqSeq& hsss,
                         const std::string& indent=empty_string);

} // ~namespace opencog

#endif /* OPENCOG_MINER_UTILS_H_ */
