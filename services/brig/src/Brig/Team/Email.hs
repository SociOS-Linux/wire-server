{-# LANGUAGE RecordWildCards #-}

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

module Brig.Team.Email
  ( InvitationEmail (..),
    CreatorWelcomeEmail (..),
    MemberWelcomeEmail (..),
    sendInvitationMail,
    sendCreatorWelcomeMail,
    sendMemberWelcomeMail,
  )
where

import Brig.App
import Brig.Email
import qualified Brig.Email as Email
import Brig.Team.Template
import Brig.Template
import Brig.Types
import Control.Lens (view)
import Data.Id (TeamId, idToText)
import qualified Data.Text.Ascii as Ascii
import Data.Text.Lazy (toStrict)
import Imports

-------------------------------------------------------------------------------
-- Invitation Email

sendInvitationMail :: Email -> TeamId -> Email -> InvitationCode -> Maybe Locale -> (AppIO r) ()
sendInvitationMail to tid from code loc = do
  tpl <- invitationEmail . snd <$> teamTemplates loc
  branding <- view templateBranding
  let mail = InvitationEmail to tid code from
  Email.sendMail $ renderInvitationEmail mail tpl branding

sendCreatorWelcomeMail :: Email -> TeamId -> Text -> Maybe Locale -> (AppIO r) ()
sendCreatorWelcomeMail to tid teamName loc = do
  tpl <- creatorWelcomeEmail . snd <$> teamTemplates loc
  branding <- view templateBranding
  let mail = CreatorWelcomeEmail to tid teamName
  Email.sendMail $ renderCreatorWelcomeMail mail tpl branding

sendMemberWelcomeMail :: Email -> TeamId -> Text -> Maybe Locale -> (AppIO r) ()
sendMemberWelcomeMail to tid teamName loc = do
  tpl <- memberWelcomeEmail . snd <$> teamTemplates loc
  branding <- view templateBranding
  let mail = MemberWelcomeEmail to tid teamName
  Email.sendMail $ renderMemberWelcomeMail mail tpl branding

-------------------------------------------------------------------------------
-- Invitation Email

data InvitationEmail = InvitationEmail
  { invTo :: !Email,
    invTeamId :: !TeamId,
    invInvCode :: !InvitationCode,
    invInviter :: !Email
  }

renderInvitationEmail :: InvitationEmail -> InvitationEmailTemplate -> TemplateBranding -> Mail
renderInvitationEmail InvitationEmail {..} InvitationEmailTemplate {..} branding =
  (emptyMail from)
    { mailTo = [to],
      mailHeaders =
        [ ("Subject", toStrict subj),
          ("X-Zeta-Purpose", "TeamInvitation"),
          ("X-Zeta-Code", Ascii.toText code)
        ],
      mailParts = [[plainPart txt, htmlPart html]]
    }
  where
    (InvitationCode code) = invInvCode
    from = Address (Just invitationEmailSenderName) (fromEmail invitationEmailSender)
    to = Address Nothing (fromEmail invTo)
    txt = renderTextWithBranding invitationEmailBodyText replace branding
    html = renderHtmlWithBranding invitationEmailBodyHtml replace branding
    subj = renderTextWithBranding invitationEmailSubject replace branding
    replace "url" = renderInvitationUrl invitationEmailUrl invTeamId invInvCode branding
    replace "inviter" = fromEmail invInviter
    replace x = x

renderInvitationUrl :: Template -> TeamId -> InvitationCode -> TemplateBranding -> Text
renderInvitationUrl t tid (InvitationCode c) branding =
  toStrict $ renderTextWithBranding t replace branding
  where
    replace "team" = idToText tid
    replace "code" = Ascii.toText c
    replace x = x

-------------------------------------------------------------------------------
-- Creator Welcome Email

data CreatorWelcomeEmail = CreatorWelcomeEmail
  { cwTo :: !Email,
    cwTid :: !TeamId,
    cwTeamName :: !Text
  }

renderCreatorWelcomeMail :: CreatorWelcomeEmail -> CreatorWelcomeEmailTemplate -> TemplateBranding -> Mail
renderCreatorWelcomeMail CreatorWelcomeEmail {..} CreatorWelcomeEmailTemplate {..} branding =
  (emptyMail from)
    { mailTo = [to],
      mailHeaders =
        [ ("Subject", toStrict subj),
          ("X-Zeta-Purpose", "Welcome")
        ],
      mailParts = [[plainPart txt, htmlPart html]]
    }
  where
    from = Address (Just creatorWelcomeEmailSenderName) (fromEmail creatorWelcomeEmailSender)
    to = Address Nothing (fromEmail cwTo)
    txt = renderTextWithBranding creatorWelcomeEmailBodyText replace branding
    html = renderHtmlWithBranding creatorWelcomeEmailBodyHtml replace branding
    subj = renderTextWithBranding creatorWelcomeEmailSubject replace branding
    replace "url" = creatorWelcomeEmailUrl
    replace "email" = fromEmail cwTo
    replace "team_id" = idToText cwTid
    replace "team_name" = cwTeamName
    replace x = x

-------------------------------------------------------------------------------
-- Member Welcome Email

data MemberWelcomeEmail = MemberWelcomeEmail
  { mwTo :: !Email,
    mwTid :: !TeamId,
    mwTeamName :: !Text
  }

renderMemberWelcomeMail :: MemberWelcomeEmail -> MemberWelcomeEmailTemplate -> TemplateBranding -> Mail
renderMemberWelcomeMail MemberWelcomeEmail {..} MemberWelcomeEmailTemplate {..} branding =
  (emptyMail from)
    { mailTo = [to],
      mailHeaders =
        [ ("Subject", toStrict subj),
          ("X-Zeta-Purpose", "Welcome")
        ],
      mailParts = [[plainPart txt, htmlPart html]]
    }
  where
    from = Address (Just memberWelcomeEmailSenderName) (fromEmail memberWelcomeEmailSender)
    to = Address Nothing (fromEmail mwTo)
    txt = renderTextWithBranding memberWelcomeEmailBodyText replace branding
    html = renderHtmlWithBranding memberWelcomeEmailBodyHtml replace branding
    subj = renderTextWithBranding memberWelcomeEmailSubject replace branding
    replace "url" = memberWelcomeEmailUrl
    replace "email" = fromEmail mwTo
    replace "team_id" = idToText mwTid
    replace "team_name" = mwTeamName
    replace x = x
