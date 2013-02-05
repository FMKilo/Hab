module Eval (eval, evalmode, mayberejoin) where

import Data.List
import qualified Data.Text as T
import System.Exit

-- Local modules
import Socket
import Write

-- Define any strings that we use on a regular basis (links etc)
-- should really go in a resource file rather than hard coded into the source
clilink = "http://terokarvinen.com/command_line.html"
kf1guide = "http://forum.xda-developers.com/showthread.php?t=1552547"
kf2rts = "http://forum.xda-developers.com/showthread.php?t=2035047"
moorom = "http://forum.xda-developers.com/showthread.php?t=2105077"
oneclick = "http://forum.xda-developers.com/showthread.php?t=2106463"
udevsetup = "http://forum.xda-developers.com/showthread.php?t=1475740"
pathtosystem = "/dev/block/platform/omap/omap_hsmmc.1/by-name/system"
-- Define admins and gods (gods have quit and op assignment controls)
lols =  ["l○l", "lol", "Lol", "LOL", "LOl", "l0l", "yolo", "Yolo", "YOLO"]
lolblock = ["FMKilo", "FMKilo-d2usc", "FMKilo-otter2-cm", "IngCr3at1on", "iytrix"]
gods = ["FMKilo", "FMKilo-d2usc", "FMKilo-otter2-cm"]
admins = ["FMKilo", "IngCr3at1on", "iytrix", "powerpoint45", "ppt45"]

-- Evaluate a command
--
-- SndNick -> Origin -> Msgtype -> content (command)
eval :: String -> String -> String -> String -> Net ()
eval u o _ c = do
    if isPriv o
        then do
            if isGod u
                then do
                    evalgodcmd u c
                    evaladcmd u c
                    evalprivcmd u c
                else if isAdmin u
                    then do
                    evaladcmd u c
                    evalprivcmd u c
                else evalprivcmd u c
        else evalchancmd u o c
  where
    isAdmin x = x `elem` admins
    isGod x = x `elem` gods
    isPriv x = "FMKilo-bot" == x

-- Evaluate God commands
--
-- SndNick -> content (command)
evalgodcmd :: String -> String -> Net ()
evalgodcmd u c
    | "~deftopic" `isPrefixOf` c = write ("TOPIC"++chan) (" :"++deftopic)
    | "~deop " `isPrefixOf` c = write ("MODE "++chan++" -o") (drop 6 c)
    | "~op " `isPrefixOf` c = write ("MODE "++chan++" +o") (drop 4 c)
    | "~opme" `isPrefixOf` c = write "MODE" (chan++" +o "++u)
    | "~quit" `isPrefixOf` c = write "QUIT" ":Reloading, hopefully..." >> io (exitWith ExitSuccess)
    | "~nick " `isPrefixOf` c = write "NICK " (drop 6 c)
    | "~ban " `isPrefixOf` c = write "MODE #kf2-dev +b " (drop 5 c)
    | "~unban " `isPrefixOf` c = write "MODE #kf2-dev -b " (drop 7 c)
    | "~pass " `isPrefixOf` c = pass (drop 6 c)
    | otherwise = return ()

-- Process admin evaluation in the same way as gods
evaladcmd :: String -> String -> Net ()
evaladcmd u c
    | "~commands" `isInfixOf` c = listadcom u
    | "~id " `isPrefixOf` c = privmsg (drop 4 c)
    | "~join " `isPrefixOf` c = write "JOIN" (drop 6 c)
    | "~kick " `isPrefixOf` c = write "KICK" (drop 6 c)
    | "~me " `isPrefixOf` c = privmsg ("\001ACTION "++(drop 4 c)++"\001")
    -- a cheap implementation of message, only works if you manually do the
    -- channel or nick as #example :<message>
    | "~msg " `isPrefixOf` c = write "PRIVMSG" (drop 5 c)
    | "~part " `isPrefixOf` c = write "PART" (drop 6 c)
    | "~topic " `isPrefixOf` c = write ("TOPIC "++chan) (" :"++drop 7 c)
    | otherwise = return ()

-- Evaluate commands sent as private messages
--
-- SndNick -> content (command)
evalprivcmd :: String -> String -> Net ()
evalprivcmd u c
    | "!adb" `isInfixOf` c = write "PRIVMSG" (u++" :"++udevsetup)
    | "!cli" `isInfixOf` c = write "PRIVMSG" (u++" :"++clilink)
    | "!commands" `isInfixOf` c = listcom u
    | "!fastboot" `isInfixOf` c = write "PRIVMSG" (u++" :"++udevsetup)
    | "!source" `isInfixOf` c = write "PRIVMSG" (u++" :"++source)
    | "!udev" `isInfixOf` c = write "PRIVMSG" (u++" :"++udevsetup)
    | "!voice" `isPrefixOf` c = write "MODE" ("#kf2-dev +v "++u)
    | "!devoice" `isPrefixOf` c = write "MODE" ("#kf2-dev -v "++u)
evalprivcmd _ _ = return ()
 
-- Evaluate in channel commands
--
-- SndNick -> Origin -> content (command)
evalchancmd :: String -> String -> String -> Net ()
evalchancmd u o c
    | "!pathtosystem" `isInfixOf` c = write "PRIVMSG" (o++" :"++pathtosystem)
    | "!cli" `isInfixOf` c = write "PRIVMSG" (o++" :"++clilink)
    | "!commands" `isInfixOf` c = listcom o
    | "!source" `isInfixOf` c = write "PRIVMSG" (o++" :"++source)
    | "!sleep" `isPrefixOf` c = write "PRIVMSG" (o++" :zzzzZZzZzzzZZZZzz")
evalchancmd u "#kf2-dev" c
    | "Hey Hashcode" `isInfixOf` c = write "PRIVMSG" ("#kf2-dev :You should know he's probably not even here. Fuckin' shit man, "++u ++", pull your head out of your @$$!!")
    | "hey hashcode" `isInfixOf` c = write "PRIVMSG" ("#kf2-dev :You should know he's probably not even here. Fuckin' shit man, "++u ++", pull your head out of your @$$!!")
    | "hey Hashcode" `isInfixOf` c = write "PRIVMSG" ("#kf2-dev :You should know he's probably not even here. Fuckin' shit man, "++u ++", pull your head out of your @$$!!")
    | "Hey hashcode" `isInfixOf` c = write "PRIVMSG" ("#kf2-dev :You should know he's probably not even here. Fuckin' shit man, "++u ++", pull your head out of your @$$!!")
    | "Hello FMKilo-bot" `isInfixOf` c = write "PRIVMSG" ("#kf2-dev :Hello "++u)
    | "hello FMKilo-bot" `isInfixOf` c = write "PRIVMSG" ("#kf2-dev :Hello "++u)
    | "hello fmkilo-bot" `isInfixOf` c = write "PRIVMSG" ("#kf2-dev :Hello "++u)
    | "Hello fmkilo-bot" `isInfixOf` c = write "PRIVMSG" ("#kf2-dev :Hello "++u)
    | "i have a brick" `isInfixOf` c = write "PRIVMSG" ("#kf2-dev :I'm sorry "++u ++", you're boned...")
    | "penises" `isInfixOf` c = write "PRIVMSG" ("#kf2-dev :Alright, I'm done with all of these dick references... You can all leave now. Especially you, "++u ++".")
    | "penis" `isInfixOf` c = write "PRIVMSG" ("#kf2-dev :Alright, I'm done with all of these dick references... You can all leave now. Especially you, "++u ++".")
--Random things the people in my chan wanted my bot to say and the input to get it.
    | "wait" `isInfixOf` c = write "PRIVMSG" "#kf2-dev :No, don't stop, go harder!!!"
    | "negros" `isInfixOf` c = write "PRIVMSG" "#kf2-dev :Hey now!!?"
    | "moorom" `isInfixOf` c = write "PRIVMSG" "#kf2-dev :this monster energy tastes abnormally sweet"
    | "phone booth" `isInfixOf` c = write "PRIVMSG" "#kf2-dev :it's a police box! sheesh :P"
    | "What is the answer to life, the universe and everything?" `isInfixOf` c = write "PRIVMSG" "#kf2-dev :forty-two"
    | "What is the answer to life the universe and everything?" `isInfixOf` c = write "PRIVMSG" "#kf2-dev :forty-two"
    | "What is the answer to life, the universe, and everything?" `isInfixOf` c = write "PRIVMSG" "#kf2-dev :forty-two"
    | "What is the answer to the ultimate question of life, the universe, and everything?" `isInfixOf` c = write "PRIVMSG" "#kf2-dev :forty-two"
evalchancmd "ppt45" "kf2-dev" "..." = privmsg "He understands..."
evalchancmd "powerpoint45" "kf2-dev" "..." = privmsg "He understands..."
evalchancmd u o c = do
    if kf1talk o
        then do
            if guide c
                then write "PRIVMSG" (o++" :"++kf1guide)
                else if udev c
                    then write "PRIVMSG" (o++" :"++udevsetup)
                else return ()
        else if kf2talk o
            then do
                if moo c
                    then write "PRIVMSG" (o++" :"++moorom)
                    else if onclick c
                        then write "PRIVMSG" (o++" :"++oneclick)
                    else if retstc c
                        then write "PRIVMSG" (o++" :"++kf2rts)
                    else if udev c
                        then write "PRIVMSG" (o++" :"++udevsetup)
                    else if isLolblock u
                        then return ()
                    else if isLol c
                        then write "KICK" ("#kf2-dev "++u ++" :NO LOL IN MY CHAN")
                    else return ()
        else return ()
  where
    isLolblock x = x `elem` lolblock
    isLol x = any (`elem` lols) (words x)
    guide x = x == "!guide"
    kf1talk x = x == "#kindlefire-dev"
    kf2talk x = x == "#kf2-dev"
    moo x = x == "!moorom"
    onclick x = x == "!oneclick"
    retstc x = x == "!rts"
    udev x = x == "!udev"

-- Evaluate a MODE change
--  origin -> modetype (voice, etc) -> modwho (changes whos mode?)
evalmode :: String -> String -> String -> Net ()
evalmode c "-o" "FMKilo-bot" = pass ("PRIVMSG ChanServ :op "++c++" "++nick)
evalmode c "-o" "FMKilo" = pass ("MODE"++c++" FMKilo +o ")
evalmode c "-o" "FMKilo-otter2-cm" = pass ("MODE"++c++" FMKilo-otter2-cm +o ")
evalmode c "-o" "FMKilo-d2usc" = pass ("MODE"++c++" FMKilo-d2usc +o ")
evalmode _ _ _ = return ()
-- Check who was kicked and if it was the bot, rejoin the channel in question	
mayberejoin :: String -> Net ()	
mayberejoin s = do
    if check s
        then write "JOIN" (origin s)
        else return ()
  where
    check x = "FMKilo-bot" == (whois s)
    origin = (!! 2) . words
    whois = (!! 3) . words
