/*
 * MinerLogger.cc
 *
 * Copyright (C) 2020 OpenCog Foundation
 *
 * Author: Kasim
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

#include "MinerLogger.h"

using namespace opencog;

// Create and return the single instance
Logger& opencog::miner_logger()
{
	auto miner_logger_instantiate = []() {
		Logger tmp;
		tmp.set_component("Miner");
		return tmp;
	};
	static Logger miner_instance(miner_logger_instantiate());
	return miner_instance;
}
