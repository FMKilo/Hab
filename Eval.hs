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
eval _ _ x
    | "MODE #kf2-dev -o FMKilo-bot" `isPrefixOf` x = write "PRIVMSG chanserv :op #kf2-dev FMKilo-bot" (drop 27 x)
eval y _ x
    | "!deop FMKilo" `isPrefixOf` x = return ()
-- Commands specific to one nick. (Nick is case sensative.)
eval "FMKilo" _ x
    | "l○l" `isPrefixOf` x = return ()
    | "lol" `isPrefixOf` x = return ()
    | "Lol" `isPrefixOf` x = return ()
    | "LOL" `isPrefixOf` x = return ()
    | "el oh el" `isPrefixOf` x = return ()
    | "l o l" `isPrefixOf` x = return ()
    | "l0l" `isPrefixOf` x = return ()
    | "L O L" `isPrefixOf` x = return ()
    | "LOl" `isPrefixOf` x = return ()
    | "L O l" `isPrefixOf` x = return ()
    | "!id " `isPrefixOf` x = privmsg (drop 4 x)
    | "!join " `isPrefixOf` x = write "JOIN" (drop 6 x)
    | "!kick " `isPrefixOf` x = write "KICK" (drop 6 x)
    | "!msg " `isPrefixOf` x = write "PRIVMSG" (drop 5 x)
    | "!part " `isPrefixOf` x = write "PART" (drop 6 x)
    | "!topic " `isPrefixOf` x = write ("PRIVMSG chanserv :topic "++chan) (drop 7 x)
    | "!me " `isPrefixOf` x = privmsg ("\001ACTION "++(drop 4 x)++"\001")
    | "!opme" `isPrefixOf` x = write "MODE" "#kf2-dev +o FMKilo"
    | "!op " `isPrefixOf` x = write "MODE #kf2-dev +o " (drop 4 x)
    | "!deop " `isPrefixOf` x = write "MODE #kf2-dev -o " (drop 6 x)
    | "!nick " `isPrefixOf` x = write "NICK " (drop 6 x)
eval "FMKilo-d2usc" _ x
    | "l○l" `isPrefixOf` x = return ()
    | "lol" `isPrefixOf` x = return ()
    | "Lol" `isPrefixOf` x = return ()
    | "LOL" `isPrefixOf` x = return ()
    | "el oh el" `isPrefixOf` x = return ()
    | "l o l" `isPrefixOf` x = return ()
    | "l0l" `isPrefixOf` x = return ()
    | "L O L" `isPrefixOf` x = return ()
    | "LOl" `isPrefixOf` x = return ()
    | "L O l" `isPrefixOf` x = return ()
    | "!id " `isPrefixOf` x = privmsg (drop 4 x)
    | "!join " `isPrefixOf` x = write "JOIN" (drop 6 x)
    | "!kick " `isPrefixOf` x = write "KICK" (drop 6 x)
    | "!msg " `isPrefixOf` x = write "PRIVMSG" (drop 5 x)
    | "!part " `isPrefixOf` x = write "PART" (drop 6 x)
    | "!me " `isPrefixOf` x = privmsg ("\001ACTION "++(drop 4 x)++"\001")
    | "!topic " `isPrefixOf` x = write ("TOPIC "++chan) (" :"++drop 7 x)
    | "!opme" `isPrefixOf` x = write "MODE" "#kf2-dev +o FMKilo-d2usc"
    | "!op " `isPrefixOf` x = write "MODE #kf2-dev +o " (drop 4 x)
    | "!deop " `isPrefixOf` x = write "MODE #kf2-dev -o " (drop 6 x)
eval "FMKilo-otter2-cm" _ x
    | "l○l" `isPrefixOf` x = return ()
    | "lol" `isPrefixOf` x = return ()
    | "Lol" `isPrefixOf` x = return ()
    | "LOL" `isPrefixOf` x = return ()
    | "el oh el" `isPrefixOf` x = return ()
    | "l o l" `isPrefixOf` x = return ()
    | "l0l" `isPrefixOf` x = return ()
    | "L O L" `isPrefixOf` x = return ()
    | "LOl" `isPrefixOf` x = return ()
    | "L O l" `isPrefixOf` x = return ()
    | "!topic " `isPrefixOf` x = write ("PRIVMSG chanserv :topic "++chan) (drop 7 x)
    | "!id " `isPrefixOf` x = privmsg (drop 4 x)
    | "!join " `isPrefixOf` x = write "JOIN" (drop 6 x)
    | "!kick " `isPrefixOf` x = write "KICK" (drop 6 x)
    | "!msg " `isPrefixOf` x = write "PRIVMSG" (drop 5 x)
    | "!me " `isPrefixOf` x = privmsg ("\001ACTION "++(drop 4 x)++"\001")
    | "!part " `isPrefixOf` x = write "PART" (drop 6 x)
    | "!opme" `isPrefixOf` x = write "MODE" "#kf2-dev +o FMKilo-otter1-cm"
    | "!op " `isPrefixOf` x = write "MODE #kf2-dev +o " (drop 4 x)
    | "!deop " `isPrefixOf` x = write "MODE #kf2-dev -o " (drop 6 x)
eval "IngCr3at1on" _ x
    | "!me " `isPrefixOf` x = privmsg ("\001ACTION "++(drop 4 x)++"\001")
    | "!id " `isPrefixOf` x = privmsg (drop 4 x)
    | "!join " `isPrefixOf` x = write "JOIN" (drop 6 x)
    | "!kick " `isPrefixOf` x = write "KICK" (drop 6 x)
    | "!msg " `isPrefixOf` x = write "PRIVMSG" (drop 5 x)
    | "!opme" `isPrefixOf` x = write "MODE" "#kf2-dev +o IngCr3at1on"
    | "!op " `isPrefixOf` x = write "MODE #kf2-dev +o " (drop 4 x)
    | "!deop " `isPrefixOf` x = write "MODE #kf2-dev -o " (drop 6 x)
eval "iytrix" _ x
    | "!me " `isPrefixOf` x = privmsg ("\001ACTION "++(drop 4 x)++"\001")
    | "!id " `isPrefixOf` x = privmsg (drop 4 x)
    | "!join " `isPrefixOf` x = write "JOIN" (drop 6 x)
    | "!kick " `isPrefixOf` x = write "KICK" (drop 6 x)
    | "!msg " `isPrefixOf` x = write "PRIVMSG" (drop 5 x)
    | "!opme" `isPrefixOf` x = write "MODE" "#kf2-dev +o iytrix"
    | "!op " `isPrefixOf` x = write "MODE #kf2-dev +o " (drop 4 x)
    | "!deop " `isPrefixOf` x = write "MODE #kf2-dev -o " (drop 6 x)
eval "powerpoint45" _ x
    | "!me " `isPrefixOf` x = privmsg "\001ACTION slaps powerpoint45\001"
    | "!id " `isPrefixOf` x = privmsg (drop 4 x)
    | "!join " `isPrefixOf` x = write "JOIN" (drop 6 x)
    | "!kick " `isPrefixOf` x = write "KICK" (drop 6 x)
    | "!msg " `isPrefixOf` x = write "PRIVMSG" (drop 5 x)
    | "!opme" `isPrefixOf` x = write "MODE" "#kf2-dev +o powerpoint45"
    | "!op " `isPrefixOf` x = write "MODE #kf2-dev +o " (drop 4 x)
    | "!deop " `isPrefixOf` x = write "MODE #kf2-dev -o " (drop 6 x)
eval "ppt45" _ x
    | "!me " `isPrefixOf` x = privmsg "\001ACTION slaps ppt45!\001"
    | "!id " `isPrefixOf` x = privmsg (drop 4 x)
    | "!join " `isPrefixOf` x = write "JOIN" (drop 6 x)
    | "!kick " `isPrefixOf` x = write "KICK" (drop 6 x)
    | "!msg " `isPrefixOf` x = write "PRIVMSG" (drop 5 x)
    | "!opme" `isPrefixOf` x = write "MODE" "#kf2-dev +o ppt45"
    | "!op " `isPrefixOf` x = write "MODE #kf2-dev +o " (drop 4 x)
    | "!deop " `isPrefixOf` x = write "MODE #kf2-dev -o " (drop 6 x)
--Conversational arguments that use the sender's name in some way
eval y _ x
    | "Hey Hashcode" `isPrefixOf` x = write "PRIVMSG" ("#kf2-dev :You should know he's probably not even here. Fuckin' shit man, "++y ++", pull your head out of your @$$!!")
    | "hey hashcode" `isPrefixOf` x = write "PRIVMSG" ("#kf2-dev :You should know he's probably not even here. Fuckin' shit man, "++y ++", pull your head out of your @$$!!")
    | "hey Hashcode" `isPrefixOf` x = write "PRIVMSG" ("#kf2-dev :You should know he's probably not even here. Fuckin' shit man, "++y ++", pull your head out of your @$$!!")
    | "Hey hashcode" `isPrefixOf` x = write "PRIVMSG" ("#kf2-dev :You should know he's probably not even here. Fuckin' shit man, "++y ++", pull your head out of your @$$!!")
    | "Hello FMKilo-bot" `isPrefixOf` x = write "PRIVMSG" ("#kf2-dev :Hello "++y)
    | "hello FMKilo-bot" `isPrefixOf` x = write "PRIVMSG" ("#kf2-dev :Hello "++y)
    | "hello fmkilo-bot" `isPrefixOf` x = write "PRIVMSG" ("#kf2-dev :Hello "++y)
    | "Hello fmkilo-bot" `isPrefixOf` x = write "PRIVMSG" ("#kf2-dev :Hello "++y)
    | "i have a brick" `isPrefixOf` x = write "PRIVMSG" ("#kf2-dev :I'm sorry "++y ++", you're boned...")
    | "penises" `isPrefixOf` x = write "PRIVMSG" ("#kf2-dev :Alright, I'm done with all of these dick references... You can all leave now. Especially you, "++y ++".")
    | "penis" `isPrefixOf` x = write "PRIVMSG" ("#kf2-dev :Alright, I'm done with all of these dick references... You can all leave now. Especially you, "++y ++".")
--Random things the people in my chan wanted my bot to say and the input to get it.
    | "wait" `isPrefixOf` x = write "PRIVMSG" "#kf2-dev :No, don't stop, go harder!!!"
    | "negros" `isPrefixOf` x = write "PRIVMSG" "#kf2-dev :Hey now!!?"
    | "moorom" `isPrefixOf` x = write "PRIVMSG" "#kf2-dev :this monster energy tastes abnormally sweet"
    | "phone booth" `isPrefixOf` x = write "PRIVMSG" "#kf2-dev :it's a police box! sheesh :P"
    | "What is the answer to life, the universe and everything?" `isPrefixOf` x = write "PRIVMSG" "#kf2-dev :forty-two"
    | "What is the answer to life the universe and everything?" `isPrefixOf` x = write "PRIVMSG" "#kf2-dev :forty-two"
    | "What is the answer to life, the universe, and everything?" `isPrefixOf` x = write "PRIVMSG" "#kf2-dev :forty-two"
    | "What is the answer to the ultimate question of life, the universe, and everything?" `isPrefixOf` x = write "PRIVMSG" "#kf2-dev :forty-two"
    | "!sleep" `isPrefixOf` x = write "PRIVMSG" "#kf2-dev :zzzzzzzzzzzzzzzzzz"
--Commands that do something other than play around with text
    | "!voice" `isPrefixOf` x = write "MODE" ("#kf2-dev +v "++y)
    | "!devoice" `isPrefixOf` x = write "MODE" ("#kf2-dev -v "++y)
    --Don't say lol at the beginning of a message in #kf2-dev. Eventually, this will apply to lol in all forms in all places within the channel.
    | "l○l" `isPrefixOf` x = write "KICK" ("#kf2-dev "++y ++" :NO LOL IN MY CHAN")
    | "lol" `isPrefixOf` x = write "KICK" ("#kf2-dev "++y ++" :NO LOL IN MY CHAN")
    | "Lol" `isPrefixOf` x = write "KICK" ("#kf2-dev "++y ++" :NO LOL IN MY CHAN")
    | "LOL" `isPrefixOf` x = write "KICK" ("#kf2-dev "++y ++" :NO LOL IN MY CHAN")
    | "el oh el" `isPrefixOf` x = write "KICK" ("#kf2-dev "++y ++" :NO LOL IN MY CHAN")
    | "l o l" `isPrefixOf` x = write "KICK" ("#kf2-dev "++y ++" :NO LOL IN MY CHAN")
    | "l0l" `isPrefixOf` x = write "KICK" ("#kf2-dev "++y ++" :NO LOL IN MY CHAN")
    | "L O L" `isPrefixOf` x = write "KICK" ("#kf2-dev "++y ++" :NO LOL IN MY CHAN")
    | "LOl" `isPrefixOf` x = write "KICK" ("#kf2-dev "++y ++" :NO LOL IN MY CHAN")
    | "L O l" `isPrefixOf` x = write "KICK" ("#kf2-dev "++y ++" :NO LOL IN MY CHAN")
    | "!source" `isPrefixOf` x = write "PRIVMSG" ("#kf2-dev :"++source)

eval _ _ _ = return () -- ignore everything else
