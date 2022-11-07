module SectionModule(Section(..)) where

data Section = Section{sectionId::Int
                      ,name::String
                      }

instance Show Section where
   show section = mconcat [ show $ sectionId section
                       , ".\t|"
                       , name section
                       ]