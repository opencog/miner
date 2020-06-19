/*
 * MinerUtils.cc
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

#include "MinerUtils.h"

#include <opencog/util/dorepeat.h>
#include <opencog/util/random.h>
#include <opencog/util/algorithm.h>

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/core/LambdaLink.h>
#include <opencog/atoms/core/RewriteLink.h>
#include <opencog/atoms/core/PresentLink.h>
#include <opencog/atoms/core/VariableSet.h>
#include <opencog/atoms/core/VariableList.h>
#include <opencog/atoms/core/NumberNode.h>
#include <opencog/atoms/core/FindUtils.h>
#include <opencog/atoms/core/TypeUtils.h>
#include <opencog/atoms/core/UnorderedLink.h>
#include <opencog/atoms/pattern/PatternLink.h>
#include <opencog/atoms/pattern/GetLink.h>
#include <opencog/query/Satisfier.h>

#include <boost/range/algorithm/transform.hpp>
#include <boost/range/algorithm/unique.hpp>
#include <boost/range/algorithm/sort.hpp>
#include <boost/range/algorithm_ext/erase.hpp>
#include <boost/numeric/conversion/cast.hpp>
#include <boost/algorithm/cxx11/all_of.hpp>
#include <boost/algorithm/cxx11/any_of.hpp>

namespace opencog
{

HandleSetSeq MinerUtils::shallow_abstract(const Valuations& valuations,
                                          unsigned ms)
{
	// Base case
	if (valuations.no_focus())
		return HandleSetSeq();

	// Recursive case
	HandleSetSeq shabs_per_var{focus_shallow_abstract(valuations, ms)};
	valuations.inc_focus_variable();
	HandleSetSeq remaining = shallow_abstract(valuations, ms);
	valuations.dec_focus_variable();
	append(shabs_per_var, remaining);
	return shabs_per_var;
}

HandleSet MinerUtils::focus_shallow_abstract(const Valuations& valuations,
                                             unsigned ms)
{
	// If there are no valuations, then the result is empty by
	// convention, regardless of the minimum support threshold.
	if (valuations.empty())
		return HandleSet();

	// No more variable to specialize from
	if (valuations.no_focus())
		return HandleSet();

	HandleSet shabs;

	// Strongly connected valuations associated to the variable under
	// focus
	const SCValuations& var_scv(valuations.focus_scvaluations());

	////////////////////////////
	// Shallow abtractions    //
	////////////////////////////

	// For each valuation create an abstraction (shallow pattern) of
	// the value associated to variable, and associate the remaining
	// valuations to it.
	HandleUCounter shapats;
	// Calculate how many valuations will be encompassed by these
	// shallow abstractions
	unsigned val_count = valuations.size() / var_scv.size();
	for (const HandleSeq& valuation : var_scv.valuations) {
		const Handle& value = var_scv.focus_value(valuation);

		// If var_scv contains only one variable, then ignore shallow
		// abstractions of nodes and nullary links as they create
		// constant abstractions and
		//
		// 1. In case there is only one strongly connected component,
		//    constant patterns cannot have support > 1.
		//
		// 2. In case there are more than one strongly connected
		//    component, constant patterns are essentially useless
		//    (don't affect the support), and they will no longer
		//    reconnect, so they will remain useless.
		//
		// For these 2 reasons they can be safely ignored.
		if (valuation.size() == 1 and is_nullary(value))
			continue;

		// Otherwise generate its shallow abstraction
		if (Handle shabs = shallow_abstract_of_val(value))
			shapats[shabs] += val_count;
	}

	// Only consider shallow abstractions that reach the minimum
	// support
	for (const auto& shapat : shapats) {
		if (ms <= shapat.second) {
			set_support(shapat.first, shapat.second);
			shabs.insert(shapat.first);
		}
	}

	////////////////////////////////
	// Variable factorizations    //
	////////////////////////////////

	// Add all subsequent factorizable variables
	HandleSeq remvars = valuations.remaining_variables();
	HandleUCounter facvars;
	for (const Handle& rv : remvars) {
		// Strongly connected valuations associated to that variable
		const SCValuations& rv_scv(valuations.get_scvaluations(rv));

		// Index of rv in rv_scv
		unsigned rv_idx = rv_scv.index(rv);

		// Whether var and rv are in the same strongly connected
		// valuations (using pointer equality to speed it up)
		bool same_scv = &rv_scv == &var_scv;

		// Ref to keep track of the number of data tree instances where
		// the value of var is equal to the value to rv
		unsigned& rv_count = facvars[rv];

		// If they are in different stronly connected valuations, then
		// put all values of rv in a set, to quickly check if any
		// value is in.
		HandleUCounter rv_vals = same_scv ?
			HandleUCounter() : rv_scv.values(rv_idx);

		// Calculate how many valuations will be encompassed by this
		// variable factorization
		unsigned val_fac_count = val_count;
		if (not same_scv)
			val_fac_count /= rv_scv.size();

		for (const HandleSeq& valuation : var_scv.valuations) {
			// Value associated to var
			const Handle& val = valuation[var_scv.focus_index()];

			// If the value of var is equal to that of rv, then
			// increase rv factorization count
			if (same_scv) {
				if (content_eq(val, valuation[rv_idx])) {
					rv_count += val_fac_count;
				}
			}
			else {
				auto it = rv_vals.find(val);
				if (it != rv_vals.end()) {
					rv_count += val_fac_count * it->second;
				}
			}

			// If the minimum support has been reached, no need to
			// keep counting
			if (ms <= rv_count)
				break;
		}
	}

	// Only consider variable factorizations reaching the minimum
	// support
	for (const auto& fvar : facvars) {
		if (ms <= fvar.second) {
			set_support(fvar.first, fvar.second);
			shabs.insert(fvar.first);
		}
   }

	return shabs;
}

bool MinerUtils::is_nullary(const Handle& h)
{
	return h->is_node() or h->get_arity() == 0;
}

Handle MinerUtils::shallow_abstract_of_val(const Handle& value)
{
	// Node or empty link, nothing to abstract
	if (is_nullary(value))
		return value;

	Type tt = value->get_type();
	HandleSeq rnd_vars = gen_rand_variables(value->get_arity());
	Handle vardecl = variable_set(rnd_vars);

	// TODO: this can probably be simplified using PresentLink, would
	// entail to have RewriteLink::beta_reduce support PresentLink.

	// Links wrapped with LocalQuoteLink
	if (tt == AND_LINK or
	    tt == OR_LINK or
	    tt == NOT_LINK) {
		return lambda(vardecl, local_quote(createLink(rnd_vars, tt)));
	}

	if (tt == BIND_LINK or       // TODO: should probabably be replaced
	                             // by scope link and its subtypes
	    (tt == EVALUATION_LINK and
	     value->getOutgoingAtom(0)->get_type() == GROUNDED_PREDICATE_NODE) or
	    nameserver().isA(tt, FUNCTION_LINK) or
	    nameserver().isA(tt, VIRTUAL_LINK)) {
		// TODO: comment out the following lines when issue #1843 on the
		// atomspace repository has been fixed (see more about that
		// below).

		// // Wrap variables in UnquoteLink
		// HandleSeq uq_vars;
		// for (Handle& var : rnd_vars)
		// 	uq_vars.push_back(unquote(var));

		// return lambda(vardecl, quote(createLink(uq_vars, tt)));
		// TODO: ignore these links for now!!! In order to support them
		// we first need to address issue #1843 on the atomspace
		// repository. That is because otherwise the quotations inside
		// these patterns get wrongly consumed down the line (especially
		// while being used in the specialization rule defined in
		// rules/specialization.scm).
		return Handle::UNDEFINED;
	}

	// Links to ignore no matter what (till supported)
	if (tt == DEFINE_LINK)
		return Handle::UNDEFINED;

	// Generic non empty link, abstract away all the arguments
	return lambda(vardecl, createLink(rnd_vars, tt));
}

Handle MinerUtils::variable_set(const HandleSeq& vars)
{
	return vars.size() == 1 ? vars[0] :
		Handle(createVariableSet(std::move(HandleSeq(vars))));
}

Handle MinerUtils::lambda(const Handle& vardecl, const Handle& body)
{
	return createLink(LAMBDA_LINK, vardecl, body);
}

Handle MinerUtils::quote(const Handle& h)
{
	return createLink(QUOTE_LINK, h);
}

Handle MinerUtils::unquote(const Handle& h)
{
	return createLink(UNQUOTE_LINK, h);
}

Handle MinerUtils::local_quote(const Handle& h)
{
	return createLink(LOCAL_QUOTE_LINK, h);
}

Handle MinerUtils::compose(const Handle& pattern, const HandleMap& var2pat)
{
	if (RewriteLinkPtr sc = RewriteLinkCast(pattern))
		return remove_useless_clauses(sc->beta_reduce(var2pat));
	return pattern;
}

HandleSeq MinerUtils::get_db(const Handle& db_cpt)
{
	// Retrieve all members of db_cpt
	HandleSeq db;
	IncomingSet member_links = db_cpt->getIncomingSetByType(MEMBER_LINK);
	for (const Handle& l : member_links) {
		Handle member = l->getOutgoingAtom(0);
		if (member != db_cpt)
			db.push_back(member);
	}
	return db;
}

unsigned MinerUtils::get_uint(const Handle& h)
{
	return (unsigned)std::round(get_double(h));
}

double MinerUtils::get_double(const Handle& h)
{
	return NumberNodeCast(h)->get_value();
}

unsigned MinerUtils::support(const Handle& pattern,
                             const HandleSeq& db,
                             unsigned ms)
{
	// Partition the pattern into strongly connected components
	HandleSeq cps(get_component_patterns(pattern));

	// Likely a constant pattern
	if (cps.empty())
	    return 1;

	// Otherwise calculate the frequency of each component
	std::vector<unsigned> freqs;
	boost::transform(cps, std::back_inserter(freqs),
	                 [&](const Handle& cp)
	                 { return component_support(cp, db, ms); });

	// Return the product of all frequencies
	return boost::accumulate(freqs, 1, std::multiplies<unsigned>());
}

unsigned MinerUtils::component_support(const Handle& component,
                                       const HandleSeq& db,
                                       unsigned ms)
{
	if (totally_abstract(component))
		return db.size();
	return restricted_satisfying_set(component, db, ms)->get_arity();
}

bool MinerUtils::enough_support(const Handle& pattern,
                                const HandleSeq& db,
                                unsigned ms)
{
	return ms <= support_mem(pattern, db, ms);
}

HandleSetSeq MinerUtils::shallow_abstract(const Handle& pattern,
                                          const HandleSeq& db,
                                          unsigned ms)
{
	Valuations valuations(pattern, db);
	return shallow_abstract(valuations, ms);
}

HandleSet MinerUtils::shallow_specialize(const Handle& pattern,
                                         const HandleSeq& db,
                                         unsigned ms,
                                         unsigned mv)
{
	// Calculate all shallow abstractions of pattern
	HandleSetSeq shabs_per_var = shallow_abstract(pattern, db, ms);

	// For each variable of pattern, generate the corresponding shallow
	// specializations
	const Variables& vars = MinerUtils::get_variables(pattern);
	size_t vari = 0;
	HandleSet results;
	for (const HandleSet& shabs : shabs_per_var) {
		for (const Handle& sa : shabs) {
			Handle npat = compose(pattern, {{vars.varseq[vari], sa}});

			if (mv < get_variables(npat).size())
				continue;

			// Set the count of npat, stored in its shallow abstraction
			set_support(npat, get_support(sa));
			// Shallow_abstract should already have eliminated shallow
			// abstraction that do not have enough support.
			results.insert(npat);
		}
		vari++;
	}
	return results;
}

Handle MinerUtils::mk_body(const HandleSeq clauses)
{
	if (clauses.size() == 0)
		return Handle::UNDEFINED;
	if (use_present_link)
		return Handle(createPresentLink(std::move(clauses)));
	if (clauses.size() == 1)
		return clauses.front();
	return Handle(createLink(std::move(clauses), AND_LINK));
}

Handle MinerUtils::mk_pattern_no_vardecl(const HandleSeq& clauses)
{
	return Handle(createLambdaLink(HandleSeq{mk_body(clauses)}));
}

Handle MinerUtils::mk_pattern_filtering_vardecl(const Handle& vardecl,
                                                const HandleSeq& clauses)
{
	Handle fvd = filter_vardecl(vardecl, clauses);
	Handle body = mk_body(clauses);
	if (fvd != nullptr and body != nullptr)
		return Handle(createLambdaLink(fvd, body));
	return Handle::UNDEFINED;
}

Handle MinerUtils::mk_pattern(const Handle& vardecl,
                              const HandleSeq& clauses)
{
	Handle body = mk_body(clauses);
	return body ? Handle(createLambdaLink(vardecl, body)) : Handle::UNDEFINED;
}

HandleSeq MinerUtils::get_component_patterns(const Handle& pattern)
{
	PatternLink pl(MinerUtils::get_vardecl(pattern),
	               MinerUtils::get_body(pattern));
	HandleSeq compats;
	const HandleSeqSeq comps(pl.get_components());
	for (unsigned i = 0; i < comps.size(); ++i)
	{
		Handle comp = mk_pattern_filtering_vardecl(get_vardecl(pattern),
		                                           comps[i]);
		if (comp)
			compats.push_back(comp);
	}
	return compats;
}

HandleSeqSeq MinerUtils::get_components(const HandleSeq& clauses)
{
	return PatternLink(mk_body(clauses)).get_components();
}

HandleSeq MinerUtils::get_conjuncts(const Handle& pattern)
{
	if (pattern->get_type() == LAMBDA_LINK) {
		Handle body = get_body(pattern);
		Type bt = body->get_type();
		if (bt == AND_LINK or bt == PRESENT_LINK) {
			Handle vardecl = get_vardecl(pattern);
			HandleSeq conjs;
			for (const Handle& clause : body->getOutgoingSet()) {
				Handle conj = mk_pattern(vardecl, {clause});
				if (conj)
					conjs.push_back(conj);
			}
			return conjs;
		}
		return {pattern};
	}
	return {};
}

Handle MinerUtils::restricted_satisfying_set(const Handle& pattern,
                                             const HandleSeq& db,
                                             unsigned ms)
{
	static AtomSpace tmp_db_as; // TODO: fix to be thread safe
	tmp_db_as.clear();
	HandleSeq tmp_db;
	for (const auto& dt : db)
		tmp_db.push_back(tmp_db_as.add_atom(dt));

	// Avoid pattern matcher warning
	if (totally_abstract(pattern) and n_conjuncts(pattern) == 1)
		return tmp_db_as.add_link(SET_LINK, std::move(tmp_db));

	// Define pattern to run
	AtomSpace tmp_query_as(&tmp_db_as);
	Handle tmp_pattern = tmp_query_as.add_atom(pattern),
		vardecl = get_vardecl(tmp_pattern),
		body = get_body(tmp_pattern),
		gl = tmp_query_as.add_link(GET_LINK, vardecl, body);

	// Run pattern matcher
	SatisfyingSet sater(&tmp_db_as);
	sater.max_results = ms;
	sater.satisfy(PatternLinkCast(gl));

	QueueValuePtr qv(sater.get_result_queue());
	HandleSeq hs(qv->to_handle_seq());
	return Handle(createUnorderedLink(std::move(hs), SET_LINK));
}

bool MinerUtils::totally_abstract(const Handle& pattern)
{
	// Check whether it is an abstraction to begin with
	if (pattern->get_type() != LAMBDA_LINK)
		return false;

	// If some variables are typed then the abstraction isn't total
	const Variables& vars = get_variables(pattern);
	if (not vars._typemap.empty())
		return false;

	// Make sure the body is either a variable, or a conjunction of
	// variables
	Handle body = get_body(pattern);
	Type bt = body->get_type();
	if (bt == VARIABLE_NODE)
		return true;
	if (bt != AND_LINK and bt != PRESENT_LINK)
		return false;
	for (const Handle& ch : body->getOutgoingSet())
		if (ch->get_type() != VARIABLE_NODE)
			return false;
	return true;
}

HandleSeq MinerUtils::gen_rand_variables(size_t n)
{
	HandleSeq variables;
	dorepeat (n)
		variables.push_back(gen_rand_variable());
	return variables;
}

Handle MinerUtils::gen_rand_variable()
{
	return createNode(VARIABLE_NODE, randstr("$PM-"));
}

const Variables& MinerUtils::get_variables(const Handle& pattern)
{
	if (RewriteLinkPtr sc = RewriteLinkCast(pattern))
		return RewriteLinkCast(pattern)->get_variables();
	static Variables empty_variables;
	return empty_variables;
}

Handle MinerUtils::get_vardecl(const Handle& pattern)
{
	if (RewriteLinkPtr sc = RewriteLinkCast(pattern)) {
		Handle vardecl = sc->get_vardecl();
		if (not vardecl)
			vardecl = sc->get_variables().get_vardecl();
		return vardecl;
	}
	return Handle::UNDEFINED;
}

const Handle& MinerUtils::get_body(const Handle& pattern)
{
	if (RewriteLinkPtr sc = RewriteLinkCast(pattern))
		return sc->get_body();
	return pattern;
}

HandleSeq MinerUtils::get_clauses(const Handle& pattern)
{
	return get_clauses_of_body(get_body(pattern));
}

HandleSeq MinerUtils::get_clauses_of_body(const Handle& body)
{
	Type bt = body->get_type();
	if (bt == AND_LINK or bt == PRESENT_LINK)
		return body->getOutgoingSet();
	return {body};
}

unsigned MinerUtils::n_conjuncts(const Handle& pattern)
{
	if (pattern->get_type() == LAMBDA_LINK) {
		Type bt = get_body(pattern)->get_type();
		if (bt == AND_LINK or bt == PRESENT_LINK)
			return get_body(pattern)->get_arity();
		return 1;
	}
	return 0;
}

Handle MinerUtils::remove_useless_clauses(const Handle& pattern)
{
	Handle vardecl = get_vardecl(pattern),
		body = get_body(pattern);
	body = remove_useless_clauses(vardecl, body);
	return Handle(createLambdaLink(vardecl, body));
}

Handle MinerUtils::remove_useless_clauses(const Handle& vardecl,
                                          const Handle& body)
{
	// Remove useless clauses
	HandleSeq clauses = get_clauses(body);
	remove_useless_clauses(vardecl, clauses);
	// Reconstruct body
	return mk_body(clauses);
}

void MinerUtils::remove_useless_clauses(const Handle& vardecl, HandleSeq& clauses)
{
	remove_constant_clauses(vardecl, clauses);
	remove_redundant_subclauses(clauses);
	remove_abstract_clauses(clauses);
}

void MinerUtils::remove_constant_clauses(const Handle& vardecl, HandleSeq& clauses)
{
	// Get Variables
	VariableSetPtr vl = createVariableSet(vardecl);
	const HandleSet& vars = vl->get_variables().varset;

	// Remove constant clauses
	auto is_constant = [&](const Handle& clause) {
		return not any_unquoted_unscoped_in_tree(clause, vars); };
	boost::remove_erase_if(clauses, is_constant);
}

void MinerUtils::remove_redundant_subclauses(HandleSeq& clauses)
{
	// Check that each clause is not a subtree of another clause,
	// remove it otherwise.
	remove_if(clauses, [](const Handle& clause, const HandleSeq& others) {
			return is_unquoted_unscoped_in_any_tree(others, clause); });
}

void MinerUtils::remove_redundant_clauses(HandleSeq& clauses)
{
	boost::sort(clauses);
	typedef std::equal_to<opencog::Handle> HandleEqual;
	boost::erase(clauses,
	             boost::unique<boost::return_found_end, HandleSeq, HandleEqual>
	             (clauses, HandleEqual()));
}

void MinerUtils::remove_abstract_clauses(HandleSeq& clauses)
{
	// For each clause, for each variable of that clause, check whether
	// such clause is an abstraction of all other clauses where such
	// variable appears, if so, then it can be removed.
	remove_if(clauses, &MinerUtils::is_more_abstract_foreach_var);
}

bool MinerUtils::has_only_joint_variables(const Handle& clause,
                                          const HandleSeq& clauses)
{
	// TODO: See Surprisingness::joint_variables for help.
	return false;
}

bool MinerUtils::is_blk_syntax_more_abstract(const HandleSeq& l_blk,
                                             const HandleSeq& r_blk,
                                             const Handle& var)
{
	Handle l_pat = MinerUtils::mk_pattern_no_vardecl(l_blk);
	Handle r_pat = MinerUtils::mk_pattern_no_vardecl(r_blk);
	return is_pat_syntax_more_abstract(l_pat, r_pat, var);
}

bool MinerUtils::is_pat_syntax_more_abstract(const Handle& l_pat,
                                             const Handle& r_pat,
                                             const Handle& var)
{
	Variables l_vars = MinerUtils::get_variables(l_pat);
	Variables r_vars = MinerUtils::get_variables(r_pat);
	Handle l_body = MinerUtils::get_body(l_pat);
	Handle r_body = MinerUtils::get_body(r_pat);

	// Let's first make sure that var is both in l_pat and r_pat
	if (not l_vars.is_in_varset(var) or not r_vars.is_in_varset(var))
		return false;

	// Remove var from l_vars and r_vars to be considered as value
	// rather than variable.
	l_vars.erase(var);
	r_vars.erase(var);

	// Find all mappings from variables (except var) to terms.
	//
	// TODO: maybe this can be optimized by using matching instead of
	// unification.
	Unify unify(l_body, r_body, l_vars, r_vars);
	Unify::SolutionSet sol = unify();

	// If it is not satisfiable, l_pat is not an abstraction
	//
	// TODO: case of nary conjunctions additional care is needed
	if (not sol.is_satisfiable())
		return false;

	Unify::TypedSubstitutions tsub = unify.typed_substitutions(sol, r_body);

	// Check that for all mappings no variable in r_vars maps to a
	// value (non-variable) or var (which is viewed as value here).
	for (const auto& var2val_map : tsub)
		for (const auto& var_val : var2val_map.first)
			if (is_value(var_val, r_vars, var))
				return false;
	return true;
}

bool MinerUtils::is_pat_more_abstract(const Handle& l_pat,
                                      const Handle& r_pat,
                                      const Handle& var)
{
	HandleSeq l_clauses = MinerUtils::get_clauses(l_pat);
	HandleSeq r_clauses = MinerUtils::get_clauses(r_pat);
	HandleSeq l_scs = connected_subpattern_with_var(l_clauses, var);
	HandleSeq r_scs = connected_subpattern_with_var(r_clauses, var);
	return is_blk_more_abstract(l_scs, r_scs, var);
}

bool MinerUtils::is_blk_more_abstract(const HandleSeq& l_blk,
                                      const HandleSeq& r_blk,
                                      const Handle& var)
{
	using namespace boost::algorithm;
	HandleSeqSeq rps = powerseq_without_empty(r_blk);
	return any_of(partitions(l_blk), [&](const HandleSeqSeq& lp) {
			return any_of(rps, [&](const HandleSeq& rs) {
					return all_of(lp, [&](const HandleSeq& lb) {
							return is_blk_syntax_more_abstract(lb, rs, var);
						});
				});
		});
}

bool MinerUtils::is_more_abstract_foreach_var(const Handle& clause,
                                              const HandleSeq& others)
{
	HandleSet vars = get_free_variables(clause);
	for (const Handle& var : vars) {
		// Filter in all other clauses containing var
		HandleSeq ov;
		for (const Handle& other : others)
			if (is_free_in_tree(other, var))
				ov.push_back(other);

		// If var appears nowhere in others, then return false, because
		// it means such pattern brings something about that variable
		// that no other pattern brings, thus cannot be an abstraction.
		if (ov.empty())
			return false;

		// Check if clause is an abstraction of each clause in ov
		// relative to var
		if (not boost::algorithm::all_of(ov, [&](const Handle& other) {
					return is_blk_syntax_more_abstract({clause}, {other}, var); }))
			return false;
	}
	return true;
}

HandleSeqSeq MinerUtils::powerseq_without_empty(const HandleSeq& blk)
{
	HandleSetSet pset = powerset(HandleSet(blk.begin(), blk.end()));
	HandleSeqSeq pseq;
	for (const HandleSet& set : pset)
		if (not set.empty())
			pseq.push_back(HandleSeq(set.begin(), set.end()));
	return pseq;
}

Handle MinerUtils::alpha_convert(const Handle& pattern,
                                 const Variables& other_vars)
{
	const Variables& pattern_vars = get_variables(pattern);

	// Detect collision between pattern_vars and other_vars
	HandleMap aconv;
	for (const Handle& var : pattern_vars.varset) {
		if (other_vars.is_in_varset(var)) {
			Handle nvar;
			bool used;
			do {
				nvar = createNode(VARIABLE_NODE, randstr(var->get_name() + "-"));
				// Make sure it is not in other_vars or pattern_vars
				used = other_vars.is_in_varset(nvar) or pattern_vars.is_in_varset(nvar);
			} while (used);
			aconv[var] = nvar;
		}
	}

	// No collision
	if (aconv.empty())
		return pattern;

	// Collisions, need to alpha convert vardecl and body
	Handle nvardecl = pattern_vars.substitute_nocheck(get_vardecl(pattern), aconv);
	Handle nbody = pattern_vars.substitute_nocheck(get_body(pattern), aconv);

	// Reconstruct alpha converted pattern
	return Handle(createLambdaLink(nvardecl, nbody));
}

bool MinerUtils::is_value(const Unify::HandleCHandleMap::value_type& var_val,
                          const Variables& vars,
                          const Handle& var)
{
	return vars.is_in_varset(var_val.first)
		and (var_val.second == Unify::CHandle(var)
		     or not var_val.second.is_free_variable());
}

HandleSeqSeq MinerUtils::connected_subpatterns_with_var(
	const HandleSeqSeq& partition,
	const Handle& var)
{
	HandleSeqSeq var_partition;
	for (const HandleSeq& blk : partition) {
		HandleSeq sc_blk = connected_subpattern_with_var(blk, var);
		if (not sc_blk.empty()) {
			var_partition.push_back(sc_blk);
		}
	}
	return var_partition;
}

HandleSeq MinerUtils::connected_subpattern_with_var(const HandleSeq& blk,
                                                    const Handle& var)
{
	if (not is_free_in_any_tree(blk, var))
		return {};

	HandleSeqSeq sccs = MinerUtils::get_components(blk);
	for (const HandleSeq& scc : sccs)
		if (is_free_in_any_tree(scc, var))
			return scc;
	return {};
}

HandleSeqSeqSeq MinerUtils::combinatorial_insert(const Handle& h,
                                                 const HandleSeqSeq& hss)
{
	return combinatorial_insert(h, hss.begin(), hss.end());
}

HandleSeqSeqSeq MinerUtils::combinatorial_insert(const Handle& h,
                                                 HandleSeqSeq::const_iterator from,
                                                 HandleSeqSeq::const_iterator to)
{
	// Base case
	if (from == to)
		return {{{h}}};

	// Recursive case
	HandleSeq head = *from;       // Copy because will get modified
	HandleSeqSeqSeq rst;
	for (auto x : combinatorial_insert(h, ++from, to)) {
		x.push_back(head);
		rst.push_back(x);
	}
	head.push_back(h);
	HandleSeqSeq fst(from, to);
	fst.push_back(head);
	rst.push_back(fst);
	return rst; 
}

HandleSeqSeqSeq MinerUtils::partitions(const HandleSeq& hs)
{
	return partitions(hs.begin(), hs.end());
}

HandleSeqSeqSeq MinerUtils::partitions(HandleSeq::const_iterator from,
                                       HandleSeq::const_iterator to)
{
	// Base case
	if (from == to)
		return {{}};

	// Recursive case
	Handle head = *from;
	HandleSeqSeqSeq res;
	for (const HandleSeqSeq& partition : partitions(++from, to)) {
		HandleSeqSeqSeq subparts = combinatorial_insert(head, partition);
		res.insert(res.end(), subparts.begin(), subparts.end());
	}
	return res;
}

HandleSeqSeqSeq MinerUtils::partitions_without_pattern(const Handle& pattern)
{
	HandleSeqSeqSeq prtns = partitions(MinerUtils::get_clauses(pattern));
	prtns.resize(prtns.size() - 1);
	// prtns.resize(1); // comment this output to only consider
   //                  // singleton blocks (convenient for debugging)
	return prtns;
}

Handle MinerUtils::expand_conjunction_disconnect(const Handle& cnjtion,
                                                 const Handle& pattern)
{
	// Copy variables from cnjtion as it will be extended
	Variables cnjtion_vars = get_variables(cnjtion);

	// Alpha convert pattern, if necessary, to avoid collisions between
	// cnjtion_vars and pattern variables
	Handle acpat = alpha_convert(pattern, cnjtion_vars);

	// Extend cnjtion_vars with pattern variables
	cnjtion_vars.extend(get_variables(acpat));

	// Expand cnjtion body with pattern, flattening the body if necessary
	HandleSeq nclauses = get_clauses(cnjtion);
	append(nclauses, get_clauses(acpat));

	// Remove redundant subclauses. This can happen if there's only one
	// variable to connect, then some subclause turn out to be
	// redundant.
	remove_redundant_subclauses(nclauses);

	// Recreate expanded conjunction
	Handle nvardecl = cnjtion_vars.get_vardecl(),
		npattern = mk_pattern(nvardecl, nclauses);

	return npattern;
}

Handle MinerUtils::expand_conjunction_connect(const Handle& cnjtion,
                                              const Handle& pattern,
                                              const Handle& cnjtion_var,
                                              const Handle& pattern_var)
{
	HandleMap p2c{{pattern_var, cnjtion_var}};
	return expand_conjunction_connect(cnjtion, pattern, p2c);
}

Handle MinerUtils::expand_conjunction_connect(const Handle& cnjtion,
                                              const Handle& pattern,
                                              const HandleMap& pv2cv)
{
	// Substitute pattern variables by cnjtion variables in pattern
	Variables pattern_vars = get_variables(pattern);
	Handle npat_body = pattern_vars.substitute_nocheck(get_body(pattern), pv2cv);
	for (const auto& el : pv2cv)
		pattern_vars.erase(el.first);

	// Extend cnjtion variables with the pattern variables, except
	// mapped variables
	Variables cnjtion_vars = get_variables(cnjtion);
	cnjtion_vars.extend(pattern_vars);

	// Expand cnjtion body with npat_body, flattening cnjtion_body if necessary
	const Handle& cnjtion_body = get_body(cnjtion);
	HandleSeq nclauses = get_clauses(cnjtion_body);
	append(nclauses, get_clauses_of_body(npat_body));

	// get new variable declaration
	Handle nvardecl = cnjtion_vars.get_vardecl();

	// Remove useless clauses, such as constant, redundant or abstract
	// clauses.
	remove_useless_clauses(nvardecl, nclauses);

	// Recreate expanded conjunction
	Handle npattern = mk_pattern(nvardecl, nclauses);

	return npattern;
}

HandleSet MinerUtils::expand_conjunction_rec(const Handle& cnjtion,
                                             const Handle& pattern,
                                             const HandleSeq& db,
                                             unsigned ms,
                                             unsigned mv,
                                             const HandleMap& pv2cv,
                                             unsigned pvi)
{
	HandleSet patterns;
	const Variables& cvars = get_variables(cnjtion);
	const Variables& pvars = get_variables(pattern);
	for (; pvi < pvars.size(); pvi++) {
		for (const Handle& cv : cvars.varseq) {
			HandleMap pv2cv_ext(pv2cv);
			pv2cv_ext[pvars.varseq[pvi]] = cv;
			Handle npat = expand_conjunction_connect(cnjtion, pattern, pv2cv_ext);

			// If the number of variables is too high or the number of
			// conjuncts has dropped then it shouldn't be considered.
			if (get_variables(npat).size() <= mv and
			    n_conjuncts(cnjtion) < n_conjuncts(npat)) {

				// Insert npat in the atomspace where cnjtion and pattern
				// are, before memoizing its support.
				if (cnjtion->getAtomSpace())
					npat = cnjtion->getAtomSpace()->add_atom(npat);

				// If npat does not have enough support, any recursive
				// call will produce specializations that do not have
				// enough support, thus can be ignored.
				if (not enough_support(npat, db, ms))
					continue;

				patterns.insert(npat);
			}

			HandleSet rrs = expand_conjunction_rec(cnjtion, pattern, db, ms, mv,
			                                       pv2cv_ext, pvi + 1);
			patterns.insert(rrs.begin(), rrs.end());
		}
	}
	return patterns;
}

HandleSet MinerUtils::expand_conjunction_es_rec(const Handle& cnjtion,
                                                const Handle& pattern,
                                                const HandleSeq& db,
                                                unsigned ms,
                                                unsigned mv,
                                                const HandleMap& pv2cv,
                                                unsigned pvi)
{
	const Variables& pvars = get_variables(pattern);

	/////////////////
	// Base case   //
	/////////////////

	// If pv2cv is total (thus specialization is guarantied) then we
	// can build the conjunction.
	if (pv2cv.size() == pvars.size()) {
		Handle npat = expand_conjunction_connect(cnjtion, pattern, pv2cv);

		// If the number of variables is too high or the number of
		// conjuncts has dropped then it shouldn't be considered.
		if (mv < get_variables(npat).size() or
		    n_conjuncts(npat) <= n_conjuncts(cnjtion))
			return {};

		// Insert npat in the atomspace where cnjtion and pattern
		// are, before memoizing its support.
		if (cnjtion->getAtomSpace())
			npat = cnjtion->getAtomSpace()->add_atom(npat);

		// If npat does not have enough support, it shouldn't be
		// considered.
		if (not enough_support(npat, db, ms))
			return {};

		return {npat};
	}

	//////////////////////
	// Recursive case   //
	//////////////////////

	HandleSet patterns;
	const Variables& cvars = get_variables(cnjtion);
	for (const Handle& cv : cvars.varseq) {
		HandleMap pv2cv_ext(pv2cv);
		pv2cv_ext[pvars.varseq[pvi]] = cv;
		HandleSet rrs = expand_conjunction_es_rec(cnjtion, pattern, db, ms,
		                                          mv, pv2cv_ext, pvi + 1);
		patterns.insert(rrs.begin(), rrs.end());
	}
	return patterns;
}

HandleSet MinerUtils::expand_conjunction(const Handle& cnjtion,
                                         const Handle& pattern,
                                         const HandleSeq& db,
                                         unsigned ms,
                                         unsigned mv,
                                         bool es)
{
	// Alpha convert pattern, if necessary, to avoid collisions between
	// cnjtion variables and pattern variables
	Handle apat = alpha_convert(pattern, get_variables(cnjtion));

	// Consider all variable mappings from apat to cnjtion
	return es ?
		expand_conjunction_es_rec(cnjtion, apat, db, ms, mv)
		: expand_conjunction_rec(cnjtion, apat, db, ms, mv);
}

const Handle& MinerUtils::support_key()
{
	static Handle ck(createNode(NODE, "*-SupportValueKey-*"));
	return ck;
}

void MinerUtils::set_support(const Handle& pattern, double support)
{
	FloatValuePtr support_fv = createFloatValue(boost::numeric_cast<double>(support));
	pattern->setValue(support_key(), ValueCast(support_fv));
}

double MinerUtils::get_support(const Handle& pattern)
{
	FloatValuePtr support_fv = FloatValueCast(pattern->getValue(support_key()));
	if (support_fv)
		return support_fv->value().front();
	return -1.0;
}

double MinerUtils::support_mem(const Handle& pattern,
                               const HandleSeq& db,
                               unsigned ms)
{
	double sup = get_support(pattern);
	if (sup < 0) {
		sup = support(pattern, db, ms);
		set_support(pattern, sup);
	}
	return sup;
}

void MinerUtils::remove_if(HandleSeq& clauses,
                           std::function<bool(const Handle&, const HandleSeq&)> fun)
{
	for (auto it = clauses.begin(); it != clauses.end();) {
		// Take all clauses except *it
		HandleSeq others(clauses.begin(), it);
		others.insert(others.end(), std::next(it), clauses.end());

		// Remove if fun is true
		if (fun(*it, others))
			it = clauses.erase(it);
		else
			++it;
	}
}

std::string oc_to_string(const HandleSeqSeqSeq& hsss,
                         const std::string& indent)
{
	std::stringstream ss;
	ss << indent << "size = " << hsss.size();
	size_t i = 0;
	for (const HandleSeqSeq& hss : hsss) {
		ss << std::endl << indent << "atoms sets[" << i << "]:" << std::endl
		   << oc_to_string(hss, indent + oc_to_string_indent);
		i++;
	}
	return ss.str();
}

} // namespace opencog
