-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Galley.Effects.RemoteConversationListStore
  ( RemoteConversationListStore (..),
    listRemoteConversations,
    getRemoteConversationStatus,
  )
where

import Data.Id
import Data.Qualified
import Galley.Effects.Paging
import Galley.Types.Conversations.Members
import Imports
import Polysemy

data RemoteConversationListStore p m a where
  ListRemoteConversations ::
    UserId ->
    Maybe (PagingState p (Remote ConvId)) ->
    Int32 ->
    RemoteConversationListStore p m (Page p (Remote ConvId))
  GetRemoteConversationStatus ::
    UserId ->
    [Remote ConvId] ->
    RemoteConversationListStore p m (Map (Remote ConvId) MemberStatus)

makeSem ''RemoteConversationListStore
