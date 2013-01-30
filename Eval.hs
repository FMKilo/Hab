module Eval (eval) where

import Data.List
import System.Exit

-- Local modules
import Socket
import Write

-- Evaluate a command
--
-- SndNick -> SndFrom (channel/privchat etc) -> content (command)
eval :: String -> String -> String -> Net ()

--Commands
-- Master commands: only the owner
eval "FMKilo" _ "!quit" = write "QUIT" ":Reloading, hopefully..." >> io (exitWith ExitSuccess)
eval "FMKilo" _ "!deftopic" = write ("TOPIC "++chan) (" :"++deftopic)

--Conversational arguments that use the sender's name in some way
eval y _ x
    | "Hey Hashcode" `isPrefixOf` x = write ("PRIVMSG #kf2-dev :You should know he's probably not even here. Fuckin' shit man, "++y ++", get your head in the game!!") (drop 70000 x)
    | "hey hashcode" `isPrefixOf` x = write ("PRIVMSG #kf2-dev :You should know he's probably not even here. Fuckin' shit man, "++y ++", get your head in the game!!") (drop 70000 x)
    | "hey Hashcode" `isPrefixOf` x = write ("PRIVMSG #kf2-dev :You should know he's probably not even here. Fuckin' shit man, "++y ++", get your head in the game!!") (drop 70000 x)
    | "Hey hashcode" `isPrefixOf` x = write ("PRIVMSG #kf2-dev :You should know he's probably not even here. Fuckin' shit man, "++y ++", get your head in the game!!") (drop 70000 x)
    | "Hello FMKilo-bot" `isPrefixOf` x = write ("PRIVMSG #kf2-dev :Hello "++y) (drop 70000 x)
    | "hello FMKilo-bot" `isPrefixOf` x = write ("PRIVMSG #kf2-dev :Hello "++y) (drop 70000 x)
    | "hello fmkilo-bot" `isPrefixOf` x = write ("PRIVMSG #kf2-dev :Hello "++y) (drop 70000 x)
    | "Hello fmkilo-bot" `isPrefixOf` x = write ("PRIVMSG #kf2-dev :Hello "++y) (drop 70000 x)
    | "i have a brick" `isPrefixOf` x = write ("PRIVMSG #kf2-dev :I'm sorry "++y ++", you're boned...") (drop 70000 x)
    | "penises" `isPrefixOf` x = write ("PRIVMSG #kf2-dev :Alright, I'm done with all of these dick references... You can all leave now. Especially you, "++y) (drop 70000 x)
--Random things the people in my chan wanted my bot to say and the input to get it.
    | "wait" `isPrefixOf` x = write "PRIVMSG #kf2-dev :No, don't stop, go harder!!!" (drop 70000 x)
    | "negros" `isPrefixOf` x = write "PRIVMSG #kf2-dev :Hey now!!?" (drop 70000 x)
    | "moorom" `isPrefixOf` x = write "PRIVMSG #kf2-dev :this monster energy tastes abnormally sweet" (drop 70000 x)
    | "phone booth" `isPrefixOf` x = write "PRIVMSG #kf2-dev :it's a police box! sheesh :P" (drop 70000 x)
    | "What is the answer to life, the universe and everything?" `isPrefixOf` x = write "PRIVMSG #kf2-dev :forty-two" (drop 70000 x)
    | "!sleep" `isPrefixOf` x = write "PRIVMSG #kf2-dev :zzzzzzzzzzzzzzzzzz" (drop 6 x)
--Commands that do something other than play around with text
    | "!voice" `isPrefixOf` x = write ("MODE #kf2-dev +v "++y) (drop 70000 x)
    --Don't say lol at the beginning of a message in #kf2-dev. Eventually, this will apply to lol in all forms in all places within the channel.
    | "lol" `isPrefixOf` x = write ("KICK #kf2-dev "++y ++" :NO LOL IN MY CHAN") (drop 70000 x)
    | "!source" `isPrefixOf` x = write ("PRIVMSG #kf2-dev :"++source) (drop 70000 x)
-- Commands specific to one nick. (Nick is case sensative.)
eval "FMKilo" _ x
    | "!id " `isPrefixOf` x = privmsg (drop 4 x)
    | "!join " `isPrefixOf` x = write "JOIN" (drop 6 x)
    | "!kick " `isPrefixOf` x = write "KICK" (drop 6 x)
    | "!msg " `isPrefixOf` x = write "PRIVMSG" (drop 5 x)
    | "!part " `isPrefixOf` x = write "PART" (drop 6 x)
    | "!topic " `isPrefixOf` x = write ("PRIVMSG chanserv :topic "++chan) (drop 7 x)
    | "!me " `isPrefixOf` x = privmsg ("\001ACTION "++(drop 4 x)++"\001")
    | "!op" `isPrefixOf` x = write "PRIVMSG chanserv :flags #kf2-dev +o FMKilo" (drop 70000 x)

eval "FMKilo-d2usc" _ x
    | "!id " `isPrefixOf` x = privmsg (drop 4 x)
    | "!join " `isPrefixOf` x = write "JOIN" (drop 6 x)
    | "!kick " `isPrefixOf` x = write "KICK" (drop 6 x)
    | "!msg " `isPrefixOf` x = write "PRIVMSG" (drop 5 x)
    | "!part " `isPrefixOf` x = write "PART" (drop 6 x)
    | "!me " `isPrefixOf` x = privmsg ("\001ACTION "++(drop 4 x)++"\001")
    | "!topic " `isPrefixOf` x = write ("TOPIC "++chan) (" :"++drop 7 x)
    | "!op" `isPrefixOf` x = write "PRIVMSG chanserv :flags #kf2-dev +o FMKilo-d2usc" (drop 70000 x)

eval "FMKilo-otter2-cm" _ x
    | "!topic " `isPrefixOf` x = write ("PRIVMSG chanserv :topic "++chan) (drop 7 x)
    | "!id " `isPrefixOf` x = privmsg (drop 4 x)
    | "!join " `isPrefixOf` x = write "JOIN" (drop 6 x)
    | "!kick " `isPrefixOf` x = write "KICK" (drop 6 x)
    | "!msg " `isPrefixOf` x = write "PRIVMSG" (drop 5 x)
    | "!me " `isPrefixOf` x = privmsg ("\001ACTION "++(drop 4 x)++"\001")
    | "!part " `isPrefixOf` x = write "PART" (drop 6 x)
    | "!op" `isPrefixOf` x = write "PRIVMSG chanserv :flags #kf2-dev +o FMKilo-otter1-cm" (drop 70000 x)

eval "IngCr3at1on" _ x
    | "!me " `isPrefixOf` x = privmsg ("\001ACTION "++(drop 4 x)++"\001")
    | "!id " `isPrefixOf` x = privmsg (drop 4 x)
    | "!join " `isPrefixOf` x = write "JOIN" (drop 6 x)
    | "!kick " `isPrefixOf` x = write "KICK" (drop 6 x)
    | "!msg " `isPrefixOf` x = write "PRIVMSG" (drop 5 x)
    | "!topic " `isPrefixOf` x = write ("PRIVMSG chanserv :topic "++chan) (drop 7 x)
    | "!op" `isPrefixOf` x = write "PRIVMSG chanserv :flags #kf2-dev +o IngCr3at1on" (drop 70000 x)

eval _ _ _ = return () -- ignore everything else

