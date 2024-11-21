module Debugging 
 (traceWInfo, traceWInfo2) where
import Debug.Trace
traceWInfo :: Show a => [Char] -> a -> a
traceWInfo infoStr x = trace ("\n** "++infoStr ++" "++ show x ++"**\n" ) x

traceWInfo2 :: Show a => [Char] -> a -> b -> b
traceWInfo2 infoStr x passThrough = trace ("\n** "++infoStr ++" "++ show x ++"**\n" ) passThrough