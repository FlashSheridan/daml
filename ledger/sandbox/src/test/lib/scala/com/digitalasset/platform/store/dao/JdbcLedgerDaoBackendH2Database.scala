// Copyright (c) 2020 The DAML Authors. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.digitalasset.platform.store.dao

import com.digitalasset.platform.store.DbType
import org.scalatest.Suite

private[dao] trait JdbcLedgerDaoBackendH2Database extends JdbcLedgerDaoBackend { this: Suite =>

  override protected val jdbcUrl = "jdbc:h2:mem:static_time;db_close_delay=-1"
  override protected val dbType = DbType.H2Database

}