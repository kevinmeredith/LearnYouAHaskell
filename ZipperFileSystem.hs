import Data.List (break)  

type Name = String
type Data = String
data FSItem = File Name Data | Folder Name [FSItem] deriving (Show)

myDisk :: FSItem  
myDisk = 
    Folder "root"   
        [ File "goat_yelling_like_man.wmv" "baaaaaa"  
        , File "pope_time.avi" "god bless"  
        , Folder "pics"  
            [ File "ape_throwing_up.jpg" "bleargh"  
            , File "watermelon_smash.gif" "smash!!"  
            , File "skull_man(scary).bmp" "Yikes!"  
            ]  
        , File "dijon_poupon.doc" "best mustard"  
        , Folder "programs"  
            [ File "fartwizard.exe" "10gotofart"  
            , File "owl_bandit.dmg" "mov eax, h00t"  
            , File "not_a_virus.exe" "really not a virus"  
            , Folder "source code"  
                [ File "best_hs_prog.hs" "main = print (fix error)"  
                , File "random.hs" "main = print 4"  
                ]  
            ]  
        ] 

data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)

type FSZipper = (FSItem, [FSCrumb])

-- non-exhaustive pattern match! TODO - fix
fsUp :: FSZipper -> FSZipper
fsUp (item, FSCrumb name ls rs:bs) = (Folder name (ls ++ [item] ++ rs), bs)

mkSimpleDisk :: FSItem
mkSimpleDisk = 
	Folder "FOO" [
	              (File "a.txt" "aaa"), (File "b.txt" "bbb"), 
	                            (Folder "BAR" [File "c.txt" "ccc"])
	             ]

-- focusing on "b.txt" would require a BreadCrumb for everything above and below
--ghci> let y = fsUp (File "b.txt" "bb", [FSCrumb "foo" [File "a.txt" "aa"] []])
--ghci> y
--(Folder "foo" [File "a.txt" "aa",File "b.txt" "bb"],[])

--Here's a function that, given a name, focuses on a file or folder that's located in the current focused folder:
 
--fsTo :: Name -> FSZipper -> FSZipper  
--fsTo name (Folder foldername (f@(File filename _ ) :is)), FSCrumb name ls rs) 
--  | name == filename = (f, FSCrumb folderName ls (is ++) ) 
--  | otherwise        = 

-- not safe!
fsTo :: Name -> FSZipper -> FSZipper
fsTo name (Folder folderName items, bs) =
	let (ls, item:rs) = break (nameIs name) items
	in  (item, FSCrumb folderName ls rs:bs)

nameIs :: Name -> FSItem -> Bool
nameIs name (Folder folderName _) = name == folderName	
nameIs name (File fileName _)     = name == fileName

--ghci> fsTo "a.txt" x
--(File "a.txt" "aaa",[FSCrumb "FOO" [] [File "b.txt" "bbb",Folder "BAR" [File "c.txt" "ccc"]]])

fsRename :: Name -> FSZipper -> FSZipper
fsRename newName (File fileName content, cs) = (File newName content, cs)
fsRename newName (Folder folderName xs, cs)  = (Folder newName xs, cs)

-- my implementation
fsMakeNewItem :: FSItem -> FSZipper -> FSZipper
fsMakeNewItem item (Folder folderName xs, cs)                   = (Folder folderName (item:xs), cs)
fsMakeNewItem item (File fileName x, (FSCrumb name ls rs) : cs) = (File fileName x, (FSCrumb name ls (item:rs)) : cs)

--testing:

--ghci> let x = (Folder "FOO" [File "a.txt" "aaa",File "b.txt" "bbb",Folder "BAR" [File "c.txt" "ccc"]],[] :: [FSCrumb])

--ghci> :t x
--x :: (FSItem, [FSCrumb])

--ghci> fsMakeNewItem (Folder "folder!" []) x
--(Folder "FOO" [Folder "folder!" [],File "a.txt" "aaa",File "b.txt" "bbb",Folder "BAR" [File "c.txt" "ccc"]],[])

--ghci> fsMakeNewItem (File "newwwwwwww" "adsfdsaf") x
--(Folder "FOO" [File "newwwwwwww" "adsfdsaf",File "a.txt" "aaa",File "b.txt" "bbb",Folder "BAR" [File "c.txt" "ccc"]],[])

-- LYAH version
fsNewFile :: FSItem -> FSZipper -> FSZipper  
fsNewFile item (Folder folderName items, bs) =   
    (Folder folderName (item:items), bs)  