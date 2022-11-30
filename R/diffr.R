library(diffr)

v1 <- textConnection("
R, or r, is the 18th letter of the modern English alphabet and the ISO basic Latin alphabet. Its name in English is ar (pronounced /ˈɑːr/), plural ars,[1] or in Ireland or /ˈɔːr/.[2]
")

v2 <- textConnection("
R, or r, is the eighteenth letter of the Latin alphabet, used in the modern English alphabet, the alphabets of other western European languages and others worldwide. Its name in English is ar (pronounced /ˈɑːr/), plural ars,[1] or in Ireland or /ˈɔːr/.[2]

The letter ⟨r⟩ is the eighth most common letter in English and the fourth-most common consonant (after ⟨t⟩, ⟨n⟩, and ⟨s⟩).[3]

The letter ⟨r⟩ is used to form the ending \"-re\", which is used in certain words such as centre in some varieties of English spelling, such as British English. Canadian English also uses the \"-re\" ending, unlike American English, where the ending is usually replaced by \"-er\" (center). This does not affect pronunciation.
")

diffr(v1, v2, before = "Version 1", after = "Version 2")