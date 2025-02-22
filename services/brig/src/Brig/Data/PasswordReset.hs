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

-- | Persistent storage for password reset codes.
-- TODO: Use Brig.Data.Codes
module Brig.Data.PasswordReset
  ( createPasswordResetCode,
    verifyPasswordResetCode,
    lookupPasswordResetCode,
    deletePasswordResetCode,
    mkPasswordResetKey,
  )
where

import Brig.App (Env, currentTime)
import Brig.Data.Instances ()
import Brig.Types
import Cassandra
import Control.Lens (view)
import Data.ByteString.Conversion
import Data.Id
import Data.Text (pack)
import qualified Data.Text.Ascii as Ascii
import Data.Time.Clock
import Imports
import OpenSSL.BN (randIntegerZeroToNMinusOne)
import OpenSSL.EVP.Digest (digestBS, getDigestByName)
import OpenSSL.Random (randBytes)
import Text.Printf (printf)

maxAttempts :: Int32
maxAttempts = 3

ttl :: NominalDiffTime
ttl = 3600 -- 60 minutes

createPasswordResetCode :: (MonadClient m, MonadReader Env m) => UserId -> Either Email Phone -> m PasswordResetPair
createPasswordResetCode u target = do
  key <- liftIO $ mkPasswordResetKey u
  now <- liftIO =<< view currentTime
  code <- liftIO $ either (const genEmailCode) (const genPhoneCode) target
  retry x5 . write codeInsert $ params LocalQuorum (key, code, u, maxAttempts, ttl `addUTCTime` now, round ttl)
  return (key, code)
  where
    genEmailCode = PasswordResetCode . Ascii.encodeBase64Url <$> randBytes 24
    genPhoneCode =
      PasswordResetCode . Ascii.unsafeFromText . pack . printf "%06d"
        <$> randIntegerZeroToNMinusOne 1000000

lookupPasswordResetCode :: (MonadClient m, MonadReader Env m) => UserId -> m (Maybe PasswordResetCode)
lookupPasswordResetCode u = do
  key <- liftIO $ mkPasswordResetKey u
  now <- liftIO =<< view currentTime
  validate now =<< retry x1 (query1 codeSelect (params LocalQuorum (Identity key)))
  where
    validate now (Just (c, _, _, Just t)) | t > now = return $ Just c
    validate _ _ = return Nothing

verifyPasswordResetCode :: (MonadClient m, MonadReader Env m) => PasswordResetPair -> m (Maybe UserId)
verifyPasswordResetCode (k, c) = do
  now <- liftIO =<< view currentTime
  code <- retry x1 (query1 codeSelect (params LocalQuorum (Identity k)))
  case code of
    Just (c', u, _, Just t) | c == c' && t >= now -> return (Just u)
    Just (c', u, Just n, Just t) | n > 1 && t > now -> do
      countdown (k, c', u, n -1, t, round ttl)
      return Nothing
    Just (_, _, _, _) -> deletePasswordResetCode k >> return Nothing
    Nothing -> return Nothing
  where
    countdown = retry x5 . write codeInsert . params LocalQuorum

deletePasswordResetCode :: MonadClient m => PasswordResetKey -> m ()
deletePasswordResetCode k = retry x5 . write codeDelete $ params LocalQuorum (Identity k)

mkPasswordResetKey :: (MonadIO m) => UserId -> m PasswordResetKey
mkPasswordResetKey u = do
  d <- liftIO $ getDigestByName "SHA256" >>= maybe (error "SHA256 not found") return
  return . PasswordResetKey . Ascii.encodeBase64Url . digestBS d $ toByteString' u

-- Queries

codeInsert :: PrepQuery W (PasswordResetKey, PasswordResetCode, UserId, Int32, UTCTime, Int32) ()
codeInsert = "INSERT INTO password_reset (key, code, user, retries, timeout) VALUES (?, ?, ?, ?, ?) USING TTL ?"

codeSelect :: PrepQuery R (Identity PasswordResetKey) (PasswordResetCode, UserId, Maybe Int32, Maybe UTCTime)
codeSelect = "SELECT code, user, retries, timeout FROM password_reset WHERE key = ?"

codeDelete :: PrepQuery W (Identity PasswordResetKey) ()
codeDelete = "DELETE FROM password_reset WHERE key = ?"
