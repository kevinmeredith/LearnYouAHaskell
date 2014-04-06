-- given a first and last name, return initials
getInitials :: (String, String) -> String
getInitials (f:_, l:_) = [f] ++ [l]
getInitials (_, _) = ""
