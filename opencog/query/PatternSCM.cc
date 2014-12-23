/*
 * PatternSCM.cc
 *
 * Guile Scheme bindings for the pattern matcher.
 * Copyright (c) 2008, 2014 Linas Vepstas <linas@linas.org>
 */

#include <opencog/util/foreach.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/guile/SchemePrimitive.h>
#include <opencog/guile/SchemeSmob.h>

#include "BindLink.h"
#include "PatternMatch.h"
#include "PatternSCM.h"

using namespace opencog;

PatternWrap::PatternWrap(Handle (f)(AtomSpace*, Handle), const char* n)
	: _func(f), _name(n)
{
	define_scheme_primitive(_name, &PatternWrap::wrapper, this);
}

Handle PatternWrap::wrapper(Handle h)
{
#ifdef HAVE_GUILE
	// XXX we should also allow opt-args to be a list of handles
	AtomSpace *as = SchemeSmob::ss_get_env_as(_name);
	Handle grounded_expressions = _func(as, h);
	return grounded_expressions;
#else
	return Handle::UNDEFINED;
#endif
}

// ========================================================

PatternSCM::PatternSCM(void)
{
	// Run implication, assuming that the argument is a handle to
	// an BindLink containing variables and an ImplicationLink.
	_binders.push_back(new PatternWrap(bindlink, "cog-bind"));

	// Identical to do_bindlink above, except that it only returns the
	// first match.
	_binders.push_back(new PatternWrap(single_bindlink, "cog-bind-single"));

	// Run implication, assuming that the argument is a handle to
	// an BindLink containing variables and an ImplicationLink
	_binders.push_back(new PatternWrap(crisp_logic_bindlink, "cog-bind-crisp"));

	// Mystery function
	_binders.push_back(new PatternWrap(pln_bindlink, "cog-bind-pln"));
}

PatternSCM::~PatternSCM()
{
	foreach (PatternWrap* pw, _binders)
		delete pw;
}

