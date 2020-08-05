import System.Concurrency.Channels
import Debug.Trace

data Fuel = Dry | More (Lazy Fuel)

data Message = Add Nat Nat

data MessagePID : (iface : reqType -> Type) -> Type where
  MkMessage : PID -> MessagePID iface

data ProcState = NoRequest | Sent | Complete

data Process : (iface : reqType -> Type) ->
               Type ->
               (in_state : ProcState) ->
               (out_state : ProcState) ->
               Type where
  Request : MessagePID service_iface ->
            (msg : service_reqType) ->
            Process iface (service_iface msg) st st
  Respond : ((msg : reqType) ->
                  Process iface (iface msg) NoRequest NoRequest) ->
            Process iface (Maybe reqType) st Sent
  Spawn : Process service_iface () NoRequest Complete ->
          Process iface (Maybe (MessagePID service_iface)) st st

  Loop : Inf (Process iface a NoRequest Complete) -> Process iface a Sent Complete
  Action : IO a -> Process iface a st st

  Pure : a -> Process iface a st st
  (>>=) : Process iface a st1 st2 -> (a -> Process iface b st2 st3) -> Process iface b st1 st3

run : Fuel -> Process iface t in_state out_state -> IO (Maybe t)
run Dry _ = pure Nothing
run fuel (Request {service_iface} (MkMessage process) msg) = do
  Just chan <- connect process
       | _ => pure Nothing
  ok <- unsafeSend chan msg
  if ok
     then do
       Just x <- unsafeRecv (service_iface msg) chan
            | Nothing => pure Nothing
       pure (Just x)
     else pure Nothing
run fuel (Respond {reqType} f) = do
  Just sender <- listen 1
       | Nothing => pure (Just Nothing)
  Just msg <- unsafeRecv reqType sender
       | Nothing => pure (Just Nothing)
  Just res <- run fuel (f msg)
  unsafeSend sender res
  pure (Just (Just msg))
run (More fuel) (Loop act) = run fuel act
run fuel (Spawn proc) = do
  Just pid <- spawn (do run fuel proc
                        pure ())
       | Nothing => pure Nothing
  pure (Just (Just (MkMessage pid)))
run fuel (Action act) = do res <- act
                           pure (Just res)
run fuel (Pure val) = pure (Just val)
run fuel (act >>= next) = do
  Just x <- run fuel act
       | Nothing => pure Nothing
  run fuel (next x)

partial
forever : Fuel
forever = More forever

partial
runProc : Process iface () in_state out_state -> IO ()
runProc proc = do
  run forever proc
  pure ()

AdderType : Message -> Type
AdderType (Add x y) = Nat

NoRecv : Void -> Type
NoRecv = const Void

Service : (iface : reqType -> Type) -> Type -> Type
Service iface a = Process iface a NoRequest Complete

Client : Type -> Type
Client a = Process NoRecv a NoRequest NoRequest

procAdder : Service AdderType ()
procAdder = do
  Respond (\msg => case msg of
                        Add x y => Pure (x + y))
  Loop procAdder

procMain : Client ()
procMain = do
  Just adder_id <- Spawn procAdder
       | Nothing => Action (putStrLn "Spawn failed")
  answer <- Request adder_id (Add 2 3)
  Action (printLn answer)
