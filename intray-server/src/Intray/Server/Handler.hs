module Intray.Server.Handler
    ( intrayServer
    ) where

import Servant.Generic

import Intray.API

import Intray.Server.Types

import Intray.Server.Handler.Admin (intrayAdminServer)
import Intray.Server.Handler.DeleteAccount (serveDeleteAccount)
import Intray.Server.Handler.DeleteItem (serveDeleteItem)
import Intray.Server.Handler.GetAccountInfo (serveGetAccountInfo)
import Intray.Server.Handler.GetIntraySize (serveGetIntraySize)
import Intray.Server.Handler.GetItem (serveGetItem)
import Intray.Server.Handler.GetItemUUIDs (serveGetItemUUIDs)
import Intray.Server.Handler.GetItems (serveGetItems)
import Intray.Server.Handler.GetShowItem (serveGetShowItem)
import Intray.Server.Handler.PostAddItem (servePostAddItem)
import Intray.Server.Handler.PostSync (servePostSync)
import Intray.Server.Handler.Public (intrayPublicServer)

intrayServer :: IntraySite (AsServerT IntrayHandler)
intrayServer =
    IntraySite
    { openSite = toServant intrayOpenServer
    , adminSite = toServant intrayAdminServer
    }

intrayOpenServer :: IntrayOpenSite (AsServerT IntrayHandler)
intrayOpenServer =
    IntrayOpenSite
    { protectedSite = toServant intrayProtectedServer
    , publicSite = toServant intrayPublicServer
    }

intrayProtectedServer :: IntrayProtectedSite (AsServerT IntrayHandler)
intrayProtectedServer =
    IntrayProtectedSite
    { getShowItem = serveGetShowItem
    , getIntraySize = serveGetIntraySize
    , getItemUUIDs = serveGetItemUUIDs
    , getItems = serveGetItems
    , postAddItem = servePostAddItem
    , getItem = serveGetItem
    , deleteItem = serveDeleteItem
    , postSync = servePostSync
    , getAccountInfo = serveGetAccountInfo
    , deleteAccount = serveDeleteAccount
    }
