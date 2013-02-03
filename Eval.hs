module Eval (eval) where

import Data.List
import System.Exit

-- Local modules
import Socket
import Write

-- Evaluate a command
--
-- SndNick -> origin (channel/privchat etc) -> msgtype -> content (command)
eval :: String -> String -> String -> String -> Net ()

--Commands
-- Master commands: only the owner
eval "FMKilo" "FMKilo-bot" "PRIVMSG" "!quit" = write "QUIT" ":Reloading, hopefully..." >> io (exitWith ExitSuccess)
eval "FMKilo" _ _ "!deftopic" = write ("TOPIC "++chan) (" :"++deftopic)
eval y _ _ x
    | "!deop FMKilo" `isPrefixOf` x = return ()
    | "!kick #kf2-dev FMKilo" `isPrefixOf` x = return ()
-- Commands specific to one nick. (Nick is case sensative.)
eval "FMKilo" _ _ x
    | "l○l" `isInfixOf` x = return ()
    | "lol" `isInfixOf` x = return ()
    | "Lol" `isInfixOf` x = return ()
    | "LOL" `isInfixOf` x = return ()
    | "el oh el" `isInfixOf` x = return ()
    | "l o l" `isInfixOf` x = return ()
    | "l0l" `isInfixOf` x = return ()
    | "L O L" `isInfixOf` x = return ()
    | "LOl" `isInfixOf` x = return ()
    | "L O l" `isInfixOf` x = return ()
    | "!id " `isPrefixOf` x = privmsg (drop 4 x)
    | "!join " `isPrefixOf` x = write "JOIN" (drop 6 x)
    | "!kick " `isPrefixOf` x = write "KICK" (drop 6 x)
    | "!msg " `isPrefixOf` x = write "PRIVMSG" (drop 5 x)
    | "!part " `isPrefixOf` x = write "PART" (drop 6 x)
    | "!topic " `isPrefixOf` x = write ("TOPIC "++chan) (drop 7 x)
    | "!me " `isPrefixOf` x = privmsg ("\001ACTION "++(drop 4 x)++"\001")
    | "!opme" `isPrefixOf` x = write "MODE" "#kf2-dev +o FMKilo"
    | "!op " `isPrefixOf` x = write "MODE #kf2-dev +o " (drop 4 x)
    | "!deop " `isPrefixOf` x = write "MODE #kf2-dev -o " (drop 6 x)
    | "!nick " `isPrefixOf` x = write "NICK " (drop 6 x)
    | "!ban " `isPrefixOf` x = write "MODE #kf2-dev +b " (drop 5 x)
    | "!unban " `isPrefixOf` x = write "MODE #kf2-dev -b " (drop 7 x)
    | "!pass " `isPrefixOf` x = pass (drop 6 x)
eval "FMKilo-d2usc" _ _ x
    | "l○l" `isInfixOf` x = return ()
    | "lol" `isInfixOf` x = return ()
    | "Lol" `isInfixOf` x = return ()
    | "LOL" `isInfixOf` x = return ()
    | "el oh el" `isInfixOf` x = return ()
    | "l o l" `isInfixOf` x = return ()
    | "l0l" `isInfixOf` x = return ()
    | "L O L" `isInfixOf` x = return ()
    | "LOl" `isInfixOf` x = return ()
    | "L O l" `isInfixOf` x = return ()
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
    | "!ban " `isPrefixOf` x = write "MODE #kf2-dev +b " (drop 5 x)
    | "!unban " `isPrefixOf` x = write "MODE #kf2-dev -b " (drop 7 x)
eval "FMKilo-otter2-cm" _ _ x
    | "l○l" `isInfixOf` x = return ()
    | "lol" `isInfixOf` x = return ()
    | "Lol" `isInfixOf` x = return ()
    | "LOL" `isInfixOf` x = return ()
    | "el oh el" `isInfixOf` x = return ()
    | "l o l" `isInfixOf` x = return ()
    | "l0l" `isInfixOf` x = return ()
    | "L O L" `isInfixOf` x = return ()
    | "LOl" `isInfixOf` x = return ()
    | "L O l" `isInfixOf` x = return ()
    | "!topic " `isPrefixOf` x = write ("TOPIC "++chan) (drop 7 x)
    | "!id " `isPrefixOf` x = privmsg (drop 4 x)
    | "!join " `isPrefixOf` x = write "JOIN" (drop 6 x)
    | "!kick " `isPrefixOf` x = write "KICK" (drop 6 x)
    | "!msg " `isPrefixOf` x = write "PRIVMSG" (drop 5 x)
    | "!me " `isPrefixOf` x = privmsg ("\001ACTION "++(drop 4 x)++"\001")
    | "!part " `isPrefixOf` x = write "PART" (drop 6 x)
    | "!opme" `isPrefixOf` x = write "MODE" "#kf2-dev +o FMKilo-otter1-cm"
    | "!op " `isPrefixOf` x = write "MODE #kf2-dev +o " (drop 4 x)
    | "!deop " `isPrefixOf` x = write "MODE #kf2-dev -o " (drop 6 x)
    | "!ban " `isPrefixOf` x = write "MODE #kf2-dev +b " (drop 5 x)
    | "!unban " `isPrefixOf` x = write "MODE #kf2-dev -b " (drop 7 x)
-- The bot was kickingitself after adding the infix part... hmmm, this intrigues me.... ex::FMKilo-otter2-cm!~FMKilo@ PRIVMSG #kf2-dev : lol
-- > KICK #kf2-dev FMKilo-otter2-cm :NO LOL IN MY CHAN
-- :FMKilo-bot!~FMKilo-bo@ KICK #kf2-dev FMKilo-otter2-cm :NO LOL IN MY CHAN
-- > KICK #kf2-dev FMKilo-bot :NO LOL IN MY CHAN
-- :FMKilo-bot!~FMKilo-bo@ KICK #kf2-dev FMKilo-bot :NO LOL IN MY CHAN
-- > KICK #kf2-dev FMKilo-bot :NO LOL IN MY CHAN
-- :zelazny.freenode.net 442 FMKilo-bot #kf2-dev :You're not on that channel
eval "IngCr3at1on" _ _ x
    | "l○l" `isInfixOf` x = return ()
    | "lol" `isInfixOf` x = return ()
    | "Lol" `isInfixOf` x = return ()
    | "LOL" `isInfixOf` x = return ()
    | "el oh el" `isInfixOf` x = return ()
    | "l o l" `isInfixOf` x = return ()
    | "l0l" `isInfixOf` x = return ()
    | "L O L" `isInfixOf` x = return ()
    | "LOl" `isInfixOf` x = return ()
    | "L O l" `isInfixOf` x = return ()
    | "!me " `isPrefixOf` x = privmsg ("\001ACTION "++(drop 4 x)++"\001")
    | "!id " `isPrefixOf` x = privmsg (drop 4 x)
    | "!join " `isPrefixOf` x = write "JOIN" (drop 6 x)
    | "!kick " `isPrefixOf` x = write "KICK" (drop 6 x)
    | "!msg " `isPrefixOf` x = write "PRIVMSG" (drop 5 x)
    | "!opme" `isPrefixOf` x = write "MODE" "#kf2-dev +o IngCr3at1on"
    | "!op " `isPrefixOf` x = write "MODE #kf2-dev +o " (drop 4 x)
    | "!deop " `isPrefixOf` x = write "MODE #kf2-dev -o " (drop 6 x)
    | "!ban " `isPrefixOf` x = write "MODE #kf2-dev +b " (drop 5 x)
    | "!unban " `isPrefixOf` x = write "MODE #kf2-dev -b " (drop 7 x)
eval "iytrix" _ _ x
    | "!me " `isPrefixOf` x = privmsg ("\001ACTION "++(drop 4 x)++"\001")
    | "!id " `isPrefixOf` x = privmsg (drop 4 x)
    | "!join " `isPrefixOf` x = write "JOIN" (drop 6 x)
    | "!kick " `isPrefixOf` x = write "KICK" (drop 6 x)
    | "!msg " `isPrefixOf` x = write "PRIVMSG" (drop 5 x)
    | "!opme" `isPrefixOf` x = write "MODE" "#kf2-dev +o iytrix"
    | "!op " `isPrefixOf` x = write "MODE #kf2-dev +o " (drop 4 x)
    | "!deop " `isPrefixOf` x = write "MODE #kf2-dev -o " (drop 6 x)
eval "powerpoint45" _ _ x
    | "!me " `isPrefixOf` x = privmsg "\001ACTION slaps powerpoint45\001"
    | "!id " `isPrefixOf` x = privmsg (drop 4 x)
    | "!join " `isPrefixOf` x = write "JOIN" (drop 6 x)
    | "!kick " `isPrefixOf` x = write "KICK" (drop 6 x)
    | "!msg " `isPrefixOf` x = write "PRIVMSG" (drop 5 x)
    | "!opme" `isPrefixOf` x = write "MODE" "#kf2-dev +o powerpoint45"
    | "!op " `isPrefixOf` x = write "MODE #kf2-dev +o " (drop 4 x)
    | "!deop " `isPrefixOf` x = write "MODE #kf2-dev -o " (drop 6 x)
eval "ppt45" _ _ x
    | "!me " `isPrefixOf` x = privmsg "\001ACTION slaps ppt45!\001"
    | "!id " `isPrefixOf` x = privmsg (drop 4 x)
    | "!join " `isPrefixOf` x = write "JOIN" (drop 6 x)
    | "!kick " `isPrefixOf` x = write "KICK" (drop 6 x)
    | "!msg " `isPrefixOf` x = write "PRIVMSG" (drop 5 x)
    | "!opme" `isPrefixOf` x = write "MODE" "#kf2-dev +o ppt45"
    | "!op " `isPrefixOf` x = write "MODE #kf2-dev +o " (drop 4 x)
    | "!deop " `isPrefixOf` x = write "MODE #kf2-dev -o " (drop 6 x)
eval "ppt45" _ _ "..." = privmsg "He understands..."
eval "powerpoint45" _ _ "..." = privmsg "He understands..."

eval y "FMKilo-bot" "PRIVMSG" x
    | "!voice" `isPrefixOf` x = write "MODE" ("#kf2-dev +v "++y)
    | "!devoice" `isPrefixOf` x = write "MODE" ("#kf2-dev -v "++y)
eval y "#kf2-dev" "PRIVMSG" x
--Conversational arguments that use the sender's name in some way
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
    --Don't say lol in a message in #kf2-dev. Eventually, this currently applies to lol in some forms in all places within the channel.
    | "l○l" `isInfixOf` x = write "KICK" ("#kf2-dev "++y ++" :NO LOL IN MY CHAN")
    | "lol" `isInfixOf` x = write "KICK" ("#kf2-dev "++y ++" :NO LOL IN MY CHAN")
    | "Lol" `isInfixOf` x = write "KICK" ("#kf2-dev "++y ++" :NO LOL IN MY CHAN")
    | "LOL" `isInfixOf` x = write "KICK" ("#kf2-dev "++y ++" :NO LOL IN MY CHAN")
    | "el oh el" `isInfixOf` x = write "KICK" ("#kf2-dev "++y ++" :NO LOL IN MY CHAN")
    | "l o l" `isInfixOf` x = write "KICK" ("#kf2-dev "++y ++" :NO LOL IN MY CHAN")
    | "l0l" `isInfixOf` x = write "KICK" ("#kf2-dev "++y ++" :NO LOL IN MY CHAN")
    | "L O L" `isInfixOf` x = write "KICK" ("#kf2-dev "++y ++" :NO LOL IN MY CHAN")
    | "LOl" `isInfixOf` x = write "KICK" ("#kf2-dev "++y ++" :NO LOL IN MY CHAN")
    | "L O l" `isInfixOf` x = write "KICK" ("#kf2-dev "++y ++" :NO LOL IN MY CHAN")
    | "!source" `isPrefixOf` x = write "PRIVMSG" ("#kf2-dev :"++source)
eval _ _ _ "You're not on that channel" = pass ("JOIN "++chan)
eval _ _ _ _ = return () -- ignore everything else








--call :: String -> String -> String -> Net ()
--call "#kf2-dev" "MODE" "#kf2-dev -o FMKilo-bot" = pass "PRIVMSG Chanserv :op FMKilo-bot" 
--call "#kf2-dev" "KICK" "#kf2-dev FMKilo-bot" = pass "JOIN #kf2-dev"
