/*
 * Surprisingness.h
 *
 * Copyright (C) 2019 SingularityNET Foundation
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
#ifndef OPENCOG_SURPRISINGNESS_H_
#define OPENCOG_SURPRISINGNESS_H_

#include <opencog/util/Counter.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/core/LambdaLink.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/ure/BetaDistribution.h>

namespace opencog
{
//! a map from handle to double
typedef Counter<Handle, double> HandleCounter;

//! a map from handle to unsigned
typedef Counter<Handle, unsigned> HandleUCounter;

/**
 * Collection of tools to calculate pattern surprisingness.
 */

class Surprisingness {
public:
	// TODO: We could reframe isurp_old and isurp to use the same
	// inference tree decomposition as in the JSD case, using standard
	// deviation or such to determine the interval of the estimate.
	// This does implies to implement a dedicated truth value estimate
	// rule to make sure we can retrieve the interval thought.

	/**
	 * Calculate the I-Surprisingness as defined in
	 * https://wiki.opencog.org/w/Measuring_Surprisingness of a pattern
	 * composed of the conjunction of components over a given
	 * db. Partitions are directly defined within that function.
	 *
	 * For instance, given
	 *
	 * pattern = (Lambda
	 *             X
	 *             (And
	 *               (Inheritance X (Concept "soda-drinker"))
	 *               (Inheritance X (Concept "male"))))
	 *
	 * partitions = { { (Lambda X (Inheritance X (Concept "soda-drinker"))),
	 *                  (Lambda X (Inheritance X (Concept "male"))) } }
	 *
	 * here since there are only 2 components, there is only one way to
	 * partition it.
	 *
	 * return calculate its I-surprisingness. The components are passed
	 * in order to take into account their support (which should
	 * already be stored in them), and db in passed in order to
	 * obtain the universe count.
	 *
	 * Normalize determines whether the surprisingness is normalized
	 * according to the pattern frequency.
	 *
	 * Although mathmetically speaking partitions are sets of sets,
	 * they are encoded as lists of lists for performance reasons.
	 */
	static double isurp_old(const Handle& pattern,
	                        const HandleSeq& db,
	                        bool normalize=true);

	/**
	 * Similar to isurp_old but takes into account joint variables.
	 *
	 * For instance the probability estimate of
	 *
	 * Lambda
	 *   X Y Z
	 *   And
	 *     Inheritance X Y
	 *     Inheritance Z C
	 *
	 * is the product of probability p1 of
	 *
	 * Lambda
	 *   X Y
	 *   Inheritance X Y
	 *
	 * with probability p2 of
	 *
	 * Lambda
	 *   Z
	 *   Inheritance Z C
	 *
	 * This works fine because the two conjuncts are independent,
	 * however, the probability estimate of
	 *
	 * Lambda
	 *   X Y
	 *   And
	 *     Inheritance X Y
	 *     Inheritance Y C
	 *
	 * isn't merely p1*p2 because they have one variable in common Y.
	 *
	 * To address that we use the fact that the above pattern is
	 * equivalent to
	 *
	 * Lambda
	 *   X Y Z
	 *   And
	 *     Inheritance X Y
	 *     Inheritance Z C
	 *     Equal Y Z
	 *
	 * Then the probability estimate is p1*p2*p3, where
	 *
	 * p3 = P(Y=Z)
	 *
	 * is the probability that the value of Y is equal to the value of
	 * Z.
	 *
	 * To calculate p3 accurately one would need to produce the
	 * distribution over all values that Y can take and the
	 * distribution of over all values that Z can take, then calculate
	 * the inner product of the 2 distributions.
	 *
	 * The problem is that, first, calculating such probability
	 * distribution is expensive, and second, the resulting estimate is
	 * too accurate and thus most pattern are measured as unsurprising
	 * due to the inner product capturing the interactions at the point
	 * of contact of the variable, between the components.  Instead an
	 * estimate relying on the counts alone of values associated to
	 * given variables in given components is derived.
	 *
	 * Let's assume the same variable X appears in n difference
	 * components. Let's call these variable appearences X1 to Xn. So
	 * the goal is to estimate P(X1=...=Xi= ...=Xn).  Let's denote
	 * V(Xi) the set of values that Xi can take when its corresponding
	 * component is matched against the database alone, without
	 * any interaction of the other components. Thus |V(Xi)| is the
	 * number values that Xi takes in that standalone component.
	 *
	 * Let's now take into account the syntactic specialization
	 * relationships between each component relative to a given
	 * variable. Formally component A is syntactic specialization of
	 * component B relative to variable X, if B is a subtree of A where
	 * all variables but X have been stripped out. Conversely we say
	 * that B is a syntactic abstraction of A relative to variable
	 * X. Obviously here the variables of interest will be joint
	 * variables between A and B. The idea of establishing a
	 * variable-relative-specialization between components is to
	 * guaranty that the number of possible values that can be chosen
	 * so that the two variable occurences equate is bounded by the
	 * number of values of the variable occurence of the more abstract
	 * component. If no such relationship exists, then the number of
	 * possible values is bounded by the size of the database,
	 * which is usually higher than the actually value, and thus often
	 * a poor basis for an estimate. By performing a purely syntactic
	 * analysis the estimate can be greatly enhenced. Since the
	 * analysis is purely syntactic it does not diminish the measure of
	 * surprisingness of the pattern relative to the database. I.e. it
	 * adequatly discounts in the surprisingness measure the
	 * surprisingness of the pattern alone.
	 *
	 * ## Examples
	 *
	 * ### First Example
	 *
	 * pattern
	 * =
	 * Lambda
	 *   X Y
	 *   And
	 *     Inheritance X Y
	 *     Inheritance F Y
	 *     Inheritance G X
	 *
	 * Let's consider a partition of 3 components/blocks, each clause
	 * is a block.
	 *
	 * A = Inheritance X Y
	 * B = Inheritance F Y
	 * C = Inheritance G X
	 *
	 * All variables of this partition are joint (F and G are
	 * constants). However relative to X components A and C are
	 * independent, while relative to Y component B is a specialization
	 * of component A.
	 *
	 * Let's rewrite the component variables by explicitly showing
	 * variable occurences in components
	 *
	 * A = Inheritance X1 Y1
	 * B = Inheritance F Y2
	 * C = Inheritance G X2
	 *
	 * The specialization relationship between A and B relative to Y
	 * allows us to infer that V(Y2) is a subset of V(Y1). Thus the
	 * number of possible values that Y2 can take is bounded by
	 * |V(Y1)|.
	 *
	 * Let's calculate P(X1=X2) and P(Y1=Y2) for this pattern.
	 *
	 * P(X1=X2) = 1/|U| * 1/|U| * |U|
	 *          = 1/|U|
	 *
	 * the first 1/|U| is because each value of X1 can be any value of
	 * the universe U. The second 1/|U| is because, and since A and C
	 * are independent relative to X, each value of X2 can also be any
	 * value of U. Then we multiple by |U| because the equality may
	 * occur for each possible value of X1 or X2, so the probabilities
	 * add up. Now for P(Y1=Y2)
	 *
	 * P(Y1=Y2) = 1/|U| * 1/|V(Y1)| * |U|
	 *          = 1/|V(Y1)|
	 *
	 * 1/|U| is because Y1 in the more abstract component A can take
	 * any value of U. For any value of Y1 however, Y2 can only take a
	 * value of V(Y1) since B is a specialization of A relative to
	 * Y. Then we multiple by |U| to add up all probabilities for each
	 * values of the more abstract component A.
	 *
	 * ### Second Example
	 *
	 * pattern
	 * =
	 * Lambda
	 *   X Y Z
	 *   And
	 *     Inheritance X Y
	 *     Inheritance Z Y
	 *
	 * Assuming a partition of the 2 components
	 *
	 * A = Inheritance X Y
	 * B = Inheritance Z Y
	 *
	 * Thus after explicitly showing variable occurences
	 *
	 * A = Inheritance X Y1
	 * B = Inheritance Z Y2
	 *
	 * P(Y1=Y2) = 1/|U| * 1/|V(Y1)| * |U|
	 *          = 1/|V(Y1)|
	 *
	 * Here A and B are actually equivalent relative to Y, meaning the
	 * specialization relationship must not be strict.
	 *
	 * ### Third Example
	 *
	 * pattern
	 * =
	 * Lambda
	 *   X Y Z W
	 *   And
	 *     List X Y X
	 *     List Z W X
	 *
	 * Assuming a partition of the 2 components
	 *
	 * A = List X Y X
	 * B = List Z W X
	 *
	 * Thus after explicitly showing variable occurences
	 *
	 * A = List X1 Y X1
	 * B = List Z W X2
	 *
	 * P(X1=X2) = 1/|U| * 1/|V(X2)| * |U| = 1/|V(X2)|
	 *
	 * It doesn't matter that X1 appears twice in A, the number values
	 * it can take is still bounded by its most specialized abstraction
	 * relative to X, that is B.
	 *
	 * ## General formula
	 *
	 * Let X be a variable, with n variable occurences X1, ..., Xn in
	 * the respective components C1, ..., Cn. Without loss of
	 * generality let's assume that C1, ..., Cn are ordered such that
	 * if Ci is strictly more abstract than Cj relative to X, then
	 * i<j. This admits many orders as it doesn't impose restrictions
	 * on components that are equivalent or independent.  But it
	 * doesn't matter as it is only used to avoid cycles in the
	 * calculation of the probability estimate. Then the general
	 * formula is
	 *
	 * P(X1=...=Xn) = Prod_{j=2}^n 1/M(Xj)
	 *
	 * where M(Xj) is either
	 *
	 * 1. |V(Xi)|, the count of Xi in the most specialized component Ci
	 * that is either more abstract than or equivalent to Cj relative
	 * to X and such that i<j.
	 *
	 * 2. |U|, if no such more abstract or equivalent component exists
	 * for Xj.
	 *
	 * A proof sketch of why it is a good estimate of P(X1=...=Xn)
	 * (under independence assumption of the data) is that the
	 * syntactic specialization relationship provides a prior to
	 * discard distributions (i.e. set their prior probabilities to 0)
	 * of values of variable occurences Xi using subset relationships
	 * between V(Xi) and V(Xj).
	 *
	 * One last remark: The count |V(Xi)| can be exact or approximated.
	 * Of course the estimate will be better if the count is exact.
	 * Since such count is only consider component by component, and
	 * interactions are never used to obtain that count, having an
	 * exact count does not invalidate the surprisingness measure.
	 * However it can be computationally costly, so an option is to
	 * approximate it.  One possible approximation under independence
	 * assumptions is as follows.  Assume the component has N
	 * variables, if these variables are completely independent then
	 * the number of values of each of them is the N-th root of the
	 * component support S
	 *
	 * |V(Xi)| ~= Nth-root(S)
	 *
	 * such that the final support can be obtained by multiplying the
	 * number of values of all variables of that component.
	 *
	 * As of today the code calculates the exact count (thus is rather
	 * slow). We have not experimented with approximated counts yet.
	 */
	static double isurp(const Handle& pattern,
	                    const HandleSeq& db,
	                    bool normalize=true,
	                    double db_ratio=1.0);

	/**
	 * Return the distance between a value and an interval
	 *
	 * That is if the value, v, is higher than the upper bound, u, then it
	 * returns the distance between u and v. If v is than the lower bound
	 * l, then it returns the distance between l and v. Otherwise it
	 * returns 0.
	 */
	static double dst_from_interval(double l, double u, double v);

	/**
	 * Convert a partition block [A,B] into a pattern like
	 *
	 * Lambda
	 *   And
	 *     B
	 *     C
	 *
	 * and insert it in as.
	 */
	static Handle add_pattern(const HandleSeq& block, AtomSpace& as);

	/**
	 * Turn a partition into a sequence of subpatterns. Add then in the
	 * provided atomspace to enable memoization of their supports.
	 */
	static HandleSeq add_subpatterns(const HandleSeqSeq& partition,
	                                 const Handle& pattern,
	                                 AtomSpace& as);

	/**
	 * Return the set of variables that appear in more than one block
	 *
	 * For instance
	 *
	 * pattern
	 * =
	 * Lambda
	 *   X Y Z
	 *   Inheritance X Y
	 *   Inheritance Y Z
	 *
	 * partition
	 * =
	 * { {Inheritance X Y},
	 *   {Inheritance Y Z} }
	 *
	 * returns
	 *
	 * [Y]
	 *
	 * because it appears in two blocks.
 	 */
	static HandleSeq joint_variables(const Handle& pattern,
	                                 const HandleSeqSeq& partition);

	/**
	 * Return the number values (groundings) associated to a given variable in a
	 * block (subpatterns) w.r.t. to db.
	 */
	static unsigned value_count(const HandleSeq& block,
	                            const Handle& var,
	                            const HandleSeq& db);

	/**
	 * Return the probability distribution over value of var in the
	 * given subpattern/block against a given database.
	 */
	static HandleCounter value_distribution(const HandleSeq& block,
	                                        const Handle& var,
	                                        const HandleSeq& db);

	/**
	 * Perform the inner product of a collection of distributions.
	 *
	 * For instance
	 *
	 * dists
	 * =
	 * { {A->0.5, B->0.5},
	 *   {B->0.4, C->0.3, D->0.3} }
	 *
	 * returns
	 *
	 * 0.5*0        // A
	 * + 0.5*0.4    // B
	 * + 0*0.3      // C
	 * + 0*0.3      // D
	 * = 0.2
	 */
	static double inner_product(const std::vector<HandleCounter>& dists);

	/**
	 * Calculate the universe count of the pattern over the given db
	 */
	static double universe_count(const Handle& pattern, const HandleSeq& db);

	/**
	 * Given a pattern, a corpus and a probability, calculate the
	 * support of that pattern.
	 */
	static double prob_to_support(const Handle& pattern,
	                              const HandleSeq& db,
	                              double prob);

	/**
	 * Calculate the empirical probability of a pattern according to a
	 * database db.
	 */
	static double emp_prob(const Handle& pattern, const HandleSeq& db);

	/**
	 * Like emp_prob with memoization.
	 */
	static double emp_prob_mem(const Handle& pattern,
	                           const HandleSeq& db);

	/**
	 * Like emp_prob but subsample the db to have subsize (if db
	 * size is greater than subsize).
	 */
	static double emp_prob_subsmp(const Handle& pattern,
	                              const HandleSeq& db,
	                              unsigned subsize=UINT_MAX);

	/**
	 * Like emp_prob but uses bootstrapping for more
	 * efficiency. n_resample is the number of subsamplings taking
	 * place, and subsize is the size of each subsample.
	 */
	static double emp_prob_bs(const Handle& pattern,
	                          const HandleSeq& db,
	                          unsigned n_resample,
	                          unsigned subsize);

	/**
	 * Calculate the empirical probability of the given pattern,
	 * possibly boostrapping if necessary. The heuristic to determine
	 * whether the booststrapping should take place, and how, is
	 * calculated based on the pattern, the db size and the
	 * probability estimate of the pattern.
	 *
	 * In the version where the prob_estimate is not provided, the
	 * prob_estimate is automatically inferred. This takes additional
	 * computation.
	 *
	 * pbs stands for possibly boostrapping.
	 */
	static double emp_prob_pbs(const Handle& pattern,
	                           const HandleSeq& db,
	                           double db_ratio);
	static double emp_prob_pbs(const Handle& pattern,
	                           const HandleSeq& db,
	                           double prob_estimate,
	                           double db_ratio);

	/**
	 * Like emp_prob_pbs with memoization.
	 */
	static double emp_prob_pbs_mem(const Handle& pattern,
	                               const HandleSeq& db,
	                               double db_ratio);
	static double emp_prob_pbs_mem(const Handle& pattern,
	                               const HandleSeq& db,
	                               double prob_estimate,
	                               double db_ratio);

	/**
	 * Calculate the empirical truth value of a pattern according to a
	 * database db.
	 */
	static TruthValuePtr emp_tv(const Handle& pattern, const HandleSeq& db);

	/**
	 * Like emp_tv with memoization.
	 */
	static TruthValuePtr emp_tv_mem(const Handle& pattern,
	                                const HandleSeq& db);

	/**
	 * Like emp_tv but subsample the db to have subsize (if db
	 * size is greater than subsize).
	 */
	static TruthValuePtr emp_tv_subsmp(const Handle& pattern,
	                                   const HandleSeq& db,
	                                   unsigned subsize=UINT_MAX);

	/**
	 * Like emp_tv but uses bootstrapping for more
	 * efficiency. n_resample is the number of subsamplings taking
	 * place, and subsize is the size of each subsample.
	 */
	static TruthValuePtr emp_tv_bs(const Handle& pattern,
	                               const HandleSeq& db,
	                               unsigned n_resample,
	                               unsigned subsize);

	/**
	 * Calculate the empirical truth value of the given pattern,
	 * possibly bootstrapping if necessary. The heuristic to determine
	 * whether the bootstrapping should take place, and how, is
	 * calculated based on the pattern, the db size and the probability
	 * estimate of the pattern.
	 *
	 * pbs stands for possibly bootstrapping.
	 */
	static TruthValuePtr emp_tv_pbs(const Handle& pattern,
	                                const HandleSeq& db,
	                                double prob_estimate,
	                                double db_ratio);

	/**
	 * Like emp_tv_pbs with memoization.
	 */
	static TruthValuePtr emp_tv_pbs_mem(const Handle& pattern,
	                                    const HandleSeq& db,
	                                    double prob_estimate,
	                                    double db_ratio);

	/**
	 * Randomly subsample db so that the resulting db has size
	 * subsize.
	 */
	static HandleSeq subsmp(const HandleSeq& db, unsigned subsize);

	/**
	 * Determine the number of samples and the subsample size given a
	 * database. The goal here to subsample so that the support does
	 * not exceed the corpus size. The upper bound of the support grows
	 * exponentially with the number conjuncts and polynomially (with
	 * maximum degree the number of conjuncts) with the size of the
	 * corpus. We try first to estimate how fast the support grows
	 * using the support estimate obtained by ji_prob_est to find what
	 * corpus size should be so that the support is roughly equal to
	 * the corpus size (because we can assume that the user at least
	 * tolerates an amount of resources in space and time that is
	 * already allocated for the corpus itself).
	 *
	 * The heuristic estimating the support from the corpus size, ts,
	 * and the number of conjuncts, nc, is
	 *
	 * f(ts, nc) = alpha * ts^nc
	 *
	 * This is an incredibly bad estimate, but it is the one we're
	 * using for now.
	 *
	 * We want to find the new corpus size nts that verifies the
	 * following equation
	 *
	 * f(nts, nc) = ts
	 *
	 * Thus
	 *
	 * alpha * nts^nc = ts
	 * nts = (ts/alpha)^{1/nc}
	 *
	 * where alpha is calculated as follows
	 *
	 * alpha = support_estimate / ts^nc
	 */
	static unsigned subsmp_size(const Handle& pattern,
	                            double db_size,
	                            double support_estimate,
	                            unsigned min_subsize=10U);

	/**
	 * Calculate min and max probability estimates of a pattern by
	 * applying ji_prob_est over all its possible partitions.
	 */
	static std::pair<double, double> ji_prob_est_interval(const Handle& pattern,
	                                                      const HandleSeq& db,
	                                                      double db_ratio);

	/**
	 * Calculate probability estimate of a pattern given a partition,
	 * assuming all blocks are independent, but takes into account the
	 * joint variables (ji_prob_est stands for joint-independent
	 * probability estimate).
	 */
	static double ji_prob_est(const HandleSeqSeq& partition,
	                          const Handle& pattern,
	                          const HandleSeq& db,
	                          double db_ratio);

	/**
	 * Calculate truth value estimate of a pattern given a partition,
	 * assuming all blocks are independent, but takes into account the
	 * joint variables (ji_tv_est stands for joint independent truth
	 * value estimate).
	 */
	static TruthValuePtr ji_tv_est(const HandleSeqSeq& partition,
	                               const Handle& pattern,
	                               const HandleSeq& db);

	/**
	 * Like above but doesn't take a partition. Instead all partitions
	 * are considered, and resulting TV is averaged.
	 */
	static TruthValuePtr ji_tv_est(const Handle& pattern,
	                               const HandleSeq& db);

	/**
	 * Like above but the result is memoized.
	 */
	static TruthValuePtr ji_tv_est_mem(const Handle& pattern,
	                                   const HandleSeq& db);

	/**
	 * Return true iff the given variable has the same position (same
	 * index) in the variable declarations of both patterns.
	 */
	static bool has_same_index(const Handle& l_pat,
	                           const Handle& r_pat,
	                           const Handle& var);

	/**
	 * Tell whether 2 blocks/subpatterns are equivalent relative to a
	 * given variable. Basically, whether both block are semantically
	 * equivalent and var is in the same position in both of them.
	 *
	 * For instance
	 *
	 * l_blk = { Inh X Y }
	 * r_blk = { Inh Z Y }
	 *
	 * are equivalent relative to Y because are both are semantically
	 * equivalent (up to an alpha-conversion) and Y is used in the same
	 * place in both blocks.
	 */
	static bool is_equivalent(const HandleSeq& l_blk,
	                          const HandleSeq& r_blk,
	                          const Handle& var);

	/**
	 * List above but takes scope links instead of blocks (whether each
	 * scope link has the conjunction of clauses of its block as body).
	 */
	static bool is_equivalent(const Handle& l_pat,
	                          const Handle& r_pat,
	                          const Handle& var);

	/**
	 * Return true iff l_blk is strictly more abstract than r_blk
	 * relative to var. That is more abstract and not equivalent.
	 */
	static bool is_strictly_more_abstract(const HandleSeq& l_blk,
	                                      const HandleSeq& r_blk,
	                                      const Handle& var);

	/**
	 * Sort the partition such that if block A is strictly more
	 * abstract than block B relative var, then A occurs before B.
	 */
	static void rank_by_abstraction(HandleSeqSeq& partition, const Handle& var);

	/**
	 * For each joint variable of pattern (variable that appears in
	 * more than one partition block) calculate the probability
	 * estimate of being assigned the same value across all
	 * blocks. That implementation takes into account syntactical
	 * abstraction between blocks in order to better estimate variable
	 * occurance equality (see the comment above isurp).
	 */
	static double eq_prob(const HandleSeqSeq& partition,
	                      const Handle& pattern,
	                      const HandleSeq& db);

	/**
	 * Key of the empirical truth value
	 */
	static const Handle& emp_tv_key();

	/**
	 * Get/set the empirical truth value of the given pattern.
	 */
	static TruthValuePtr get_emp_tv(const Handle& pattern);
	static void set_emp_tv(const Handle& pattern, TruthValuePtr etv);
	static void set_emp_prob(const Handle& pattern, double ep);

	/**
	 * Key of the joint-independent truth value estimate
	 */
	static const Handle& ji_tv_est_key();

	/**
	 * Get/set the joint-independent truth value estimate of the given
	 * pattern.
	 */
	static TruthValuePtr get_ji_tv_est(const Handle& pattern);
	static void set_ji_tv_est(const Handle& pattern, TruthValuePtr etv);

	/**
	 * Given 2 TVs, typically representing the empirical probability
	 * and the probability estimate of a pattern, calculate the
	 * Jensen-Shannon distance between them.
	 */
	static double jsd(TruthValuePtr l_tv, TruthValuePtr r_tv);

	/**
	 * Given 2 cdfs (cummulative distribution functions) return their
	 * Kullback-Leibler divergence.
	 *
	 * The cdfs are described as vectors of regularly spaced right-end
	 * points. The point at the origin is ignored because it is always
	 * 0, but the last one, which is always 1, is present for
	 * completeness.
	 */
	static double kld(const std::vector<double>& l_cdf,
	                  const std::vector<double>& r_cdf);

	/**
	 * Calculate the average of 2 values, that is (l+r)/2
	 */
	static double avrg(double l, double r);

	/**
	 * Calculate the average of n values, that is (sum vs)/|vs|
	 */
	static double avrg(std::vector<double>& vs);

	/**
	 * Given a sequence of truth values, return the truth value
	 * representing their average. Note that this process looses
	 * information as the returned truth value is a simple truth value
	 * equivalent to a Beta distribution, thus cannot be multimodal.
	 */
	static TruthValuePtr avrg_tv(const TruthValueSeq& tvs);

	/**
	 * Given 2 cdfs, return their average, that is (cdf1 + cdf2)/2.
	 */
	static std::vector<double> avrg_cdf(const std::vector<double>& l_cdf,
	                                    const std::vector<double>& r_cdf);


	/**
	 * Method to convert confidence to count and vice versa of a simple
	 * truth value. Should probably be moved to SimpleTruthValue.
	 */
	static count_t confidence_to_count(confidence_t cfd);
	static confidence_t count_to_confidence(count_t cnt);

	/**
	 * Log PDF of a given beta distribution, discretized in a given
	 * number of bins.
	 */
	static void log_pdf(const BetaDistribution& bd, int bins);
};

} // ~namespace opencog

#endif /* OPENCOG_SURPRISINGNESS_H_ */
