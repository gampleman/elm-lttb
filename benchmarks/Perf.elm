module Perf exposing (main)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import LTTB
import LTTBNew
import Random


seed =
    Random.initialSeed 456789786543


fancyGetter : ( Float, Float ) -> Float
fancyGetter tuple =
    if Tuple.second tuple < 0 then
        fancyGetter (Tuple.mapSecond (\a -> -1 * a) tuple)

    else
        Tuple.second tuple


suite : Benchmark
suite =
    let
        sampleData =
            Random.step (Random.list 1000 (Random.pair (Random.float 0 1000) (Random.float -500 500))) seed
                |> Tuple.first
                |> List.sortBy Tuple.first
    in
    describe "LTTB"
        [ Benchmark.compare "1000 -> 100"
            "old"
            (\() ->
                LTTB.downsample
                    { data = sampleData
                    , threshold = 100
                    , xGetter = Tuple.first
                    , yGetter = Tuple.second
                    }
            )
            "new"
            (\() ->
                LTTBNew.downsample
                    { data = sampleData
                    , threshold = 100
                    , xGetter = Tuple.first
                    , yGetter = Tuple.second
                    }
            )
        , Benchmark.compare "1000 -> 100 with more expensive getter"
            "old"
            (\() ->
                LTTB.downsample
                    { data = sampleData
                    , threshold = 100
                    , xGetter = Tuple.first
                    , yGetter = fancyGetter
                    }
            )
            "new"
            (\() ->
                LTTBNew.downsample
                    { data = sampleData
                    , threshold = 100
                    , xGetter = Tuple.first
                    , yGetter = fancyGetter
                    }
            )
        , Benchmark.compare "1000 -> 900"
            "old"
            (\() ->
                LTTB.downsample
                    { data = sampleData
                    , threshold = 900
                    , xGetter = Tuple.first
                    , yGetter = Tuple.second
                    }
            )
            "new"
            (\() ->
                LTTBNew.downsample
                    { data = sampleData
                    , threshold = 900
                    , xGetter = Tuple.first
                    , yGetter = Tuple.second
                    }
            )
        , Benchmark.compare "1000 -> 10"
            "old"
            (\() ->
                LTTB.downsample
                    { data = sampleData
                    , threshold = 10
                    , xGetter = Tuple.first
                    , yGetter = Tuple.second
                    }
            )
            "new"
            (\() ->
                LTTBNew.downsample
                    { data = sampleData
                    , threshold = 10
                    , xGetter = Tuple.first
                    , yGetter = Tuple.second
                    }
            )
        ]


main : BenchmarkProgram
main =
    program suite
