{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell,
             FlexibleContexts,
             TypeOperators
  #-}

module BrowserExample where

import AOP.Membranes
import Network.URL
import Data.Map (Map, member, insert, (!), empty)
import Control.Monad.Zipper

import Debug.Trace

type HtmlDoc = String
type BrowserState = [(URL, HtmlDoc)]

instance Typeable URL where
   typeOf _ = mkTyConApp (mkTyCon3 "Network" "Network" "URL") []

browserSessionTag = $newTag
browserSession :: (MonadIO m, MonadState BrowserState m, OpenApp Function m) => Function [String] (m ())
browserSession = flip mkFunction browserSessionTag (\ urlList ->
               case urlList of
                    []              -> return ()
                    (inputUrl:urls) -> do
                                    let maybeUrl = importURL inputUrl
                                    if null inputUrl
                                       then return ()
                                       else case maybeUrl of
                                            Nothing  -> do browserSession # urls
                                            Just url -> do browser # url
                                                           browserSession # urls)

browserTag = $newTag
browser :: (MonadIO m, MonadState BrowserState m) => Function URL (m HtmlDoc)
browser = flip mkFunction browserTag (\ url -> do 
           (liftIO . print) $ "Processing URL: " ++ exportURL url
           pageList <- get
           let htmlDoc = "Document" ++ (show $ length pageList + 1)
           put $ (url, htmlDoc) : pageList
           return htmlDoc)

type M = AOT (MembraneT (WriterT Log (StateT BrowserState (StateT BrowserCache IO))))

runM :: M a -> IO (a, String)
runM c = evalStateT (evalStateT (runWriterT (runMembraneT (runAOT $ c) (0, emptyMembraneGraph))) []) empty

program :: M ()
program = do let cacheView        = i `vcomp` o `vcomp` o `vcomp` o
             let browserQuotaView = i `vcomp` i `vcomp` o `vcomp` r2
             let cacheQuotaView   = i `vcomp` i `vcomp` i `vcomp` o `vcomp` r2
             browserMb      <- newMembrane Just Just NoAdvice           idv
             cacheMb        <- newMembrane Just Just NarrowingAdvice    cacheView
             browserQuotaMb <- newMembrane Just Just AugmentationAdvice browserQuotaView
             cacheQuotaMb   <- newMembrane Just Just AugmentationAdvice cacheQuotaView
             register (pcCallGlobal cacheView browser) cacheAdv cacheMb
             register (pcCallGlobal browserQuotaView browser) (quotaAdv "Browser" length 2) browserQuotaMb
             register (pcCallView cacheView cacheQuotaView cachePut) (quotaAdv "Cache" length 2) cacheQuotaMb
             advise cacheMb browserMb
             advise browserQuotaMb browserMb
             advise cacheQuotaMb cacheMb
             evalIn browserMb (browserSession # ["www.google.cl", "dosisdiarias.com", "slashdot.org", "phdcomics.com"])         

main = runM program

type BrowserCache = CacheState URL HtmlDoc
type CacheState a b = Map a b
type Log = String

cacheAdv :: (MonadState (CacheState a b) m, Ord a, OpenApp Function m) => Narrowing m a b ()
cacheAdv = Narrowing (predicate, (augBefore, augAfter), replacement) where
        predicate url         = do {cache <- get; return (not (member url cache))}
        augBefore _           = return ()
        augAfter url page _   = cachePut # (url, page)
        replacement url       = do {cache <- get; return (cache ! url)}

cachePutTag = $newTag
cachePut :: (MonadState (CacheState a b) m, Ord a) => Function (a,b) (m ())
cachePut = flip mkFunction cachePutTag (\ (url,page) -> do {cacheState <- get; put (insert url page cacheState)})

quotaAdv :: (MonadReader s m, MonadWriter Log m) => String -> (s -> Int) -> Int -> Augmentation m a b ()
quotaAdv identifier getSize quota = Augmentation (augBefore, augAfter) where
        augBefore _       = do { s <- ask; tell ("[QUOTA: " ++ identifier ++ "] size before proceed: " ++ show (getSize s))}
        augAfter  _ _ _   = do s <- ask;
                               let size = getSize s
                               if size > quota
                                  then tell ("[QUOTA: " ++ identifier ++ "] Operation exceeded quota, size used: " ++ show size)
                                  else return ()
