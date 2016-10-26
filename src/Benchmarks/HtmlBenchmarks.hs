-- | This is a collection of HTML benchmarks for BlazeMarkup.
--
{-# LANGUAGE OverloadedStrings, ExistentialQuantification #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Benchmarks.HtmlBenchmarks where

import Data.Monoid (Monoid, mempty, mconcat, mappend)
import Prelude hiding (div, id)
import qualified Prelude as P

import Benchmarks.BenchmarkUtils
import Text.Blaze
import qualified Benchmarks.BenchmarkUtils as H

-- | Description of an HTML benchmark
--
data HtmlBenchmark = forall a. HtmlBenchmark
    { hName :: String       -- ^ Name.
    , hR :: (a -> Html)  -- ^ Rendering function.
    , hD :: a            -- ^ Data.
    , hH :: Html }         -- ^ Longer description.

-- | List containing all benchmarks.
--
benchmarks :: [HtmlBenchmark]
benchmarks =
    [ HtmlBenchmark "bigTable" bigTable bigTableData $
        let h = toHtml $ length bigTableData
            w = toHtml $ length $ P.head bigTableData
        in "Rendering of a big (" >> h >> "x" >> w >> ") HTML table"
    , HtmlBenchmark "basic" basic basicData
        "A simple, small basic template with a few holes to fill in"
    , HtmlBenchmark "wideTree" wideTree wideTreeData $
        "A very wide tree (" >> toHtml (length wideTreeData) >> " elements)"
    , HtmlBenchmark "wideTreeEscaping" wideTree wideTreeEscapingData $ do
        "A very wide tree (" >> toHtml (length wideTreeData) >> " elements)"
        " with lots of escaping"
    , HtmlBenchmark "deepTree" deepTree deepTreeData $ do
        "A really deep tree (" >> toHtml deepTreeData >> " nested templates)"
    , HtmlBenchmark "manyAttributes" manyAttributes manyAttributesData $ do
        "A single element with " >> toHtml (length manyAttributesData)
        " attributes."
    , HtmlBenchmark "customAttribute" customAttributes customAttributesData $
        "Creating custom attributes"
    ]

rows :: Int
rows = 1000

bigTableData :: [[Int]]
bigTableData = replicate rows [1..10]
{-# NOINLINE bigTableData #-}

basicData :: (String, String, [String])
basicData = ("Just a test", "joe", items)
{-# NOINLINE basicData #-}

items :: [String]
items = map (("Number " `mappend`) . show) [1 :: Int .. 14]
{-# NOINLINE items #-}

wideTreeData :: [String]
wideTreeData = take 5000 $
    cycle ["λf.(λx.fxx)(λx.fxx)", "These old days", "Foobar", "lol", "x ∈ A"]
{-# NOINLINE wideTreeData #-}

wideTreeEscapingData :: [String]
wideTreeEscapingData = take 1000 $
    cycle ["<><>", "\"lol\"", "<&>", "'>>'"]
{-# NOINLINE wideTreeEscapingData #-}

deepTreeData :: Int
deepTreeData = 1000
{-# NOINLINE deepTreeData #-}

manyAttributesData :: [String]
manyAttributesData = wideTreeData

customAttributesData :: [(String, String)]
customAttributesData = zip wideTreeData wideTreeData

-- | Render the argument matrix as an HTML table.
--
bigTable :: [[Int]]  -- ^ Matrix.
         -> Html     -- ^ Result.
bigTable t = table $ mconcat $ map row t
  where
    row r = tr $ mconcat $ map (td . toHtml) r

-- | Render a simple HTML page with some data.
--
basic :: (String, String, [String])  -- ^ (Title, User, Items)
      -> Html                        -- ^ Result.
basic (title', user, items') = html $ do
    H.head $ title $ toHtml title'
    body $ do
        div ! id "header" $ (h1 $ toHtml title')
        p $ "Hello, " `mappend` toHtml user `mappend` "!"
        p $ "Hello, me!"
        p $ "Hello, world!"
        h2 $ "loop"
        ol $ mconcat $ map (li . toHtml) items'
        div ! id "footer" $ mempty

-- | A benchmark producing a very wide but very shallow tree.
--
wideTree :: [String]  -- ^ Text to create a tree from.
         -> Html      -- ^ Result.
wideTree = div . mapM_ ((p ! id "foo") . toHtml)

-- | Create a very deep tree.
--
deepTree :: Int   -- ^ Depth of the tree.
         -> Html  -- ^ Result.
deepTree 0 = "foo"
deepTree n = p $ table $ tr $ td $ div $ deepTree (n - 1)

-- | Create an element with many attributes.
--
manyAttributes :: [String]  -- ^ List of attribute values.
               -> Html      -- ^ Result.
manyAttributes = foldl setAttribute img
  where
    setAttribute html' value' = html' ! id (toValue value')
    {-# INLINE setAttribute #-}

customAttributes :: [(String, String)]  -- ^ List of attribute name, value pairs
                 -> Html                -- ^ Result
customAttributes = foldl setAttribute img
  where
    setAttribute html' (name, value') =
        html' ! customAttribute (stringTag name) (toValue value')
