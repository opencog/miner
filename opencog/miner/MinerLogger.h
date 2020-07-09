/*
 * MinerLogger.h
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

#ifndef _OPENCOG_MINERLOGGER_H_
#define _OPENCOG_MINERLOGGER_H_

#include <opencog/util/Logger.h>

namespace opencog
{

// singleton instance (following Meyer's design pattern)
Logger& miner_logger();

// Macros to not evaluate the stream if log level is disabled
#define LAZY_MINER_LOG_ERROR if(miner_logger().is_error_enabled()) miner_logger().error()
#define LAZY_MINER_LOG_WARN if(miner_logger().is_warn_enabled()) miner_logger().warn()
#define LAZY_MINER_LOG_INFO if(miner_logger().is_info_enabled()) miner_logger().info()
#define LAZY_MINER_LOG_DEBUG if(miner_logger().is_debug_enabled()) miner_logger().debug()
#define LAZY_MINER_LOG_FINE if(miner_logger().is_fine_enabled()) miner_logger().fine()

} // ~namespace opencog

#endif /* _OPENCOG_MINERLOGGER_H_ */
