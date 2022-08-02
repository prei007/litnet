### Creating ppt slides with officeR

# https://www.r-bloggers.com/2020/07/how-to-create-powerpoint-reports-with-r/


library(officer)
library(magrittr) # only needed if using piping. 

pres <- read_pptx("blank.pptx")
layout_summary(pres)


# add title slide
pres <- add_slide(pres, layout = "Title Only", master = "Office Theme")

# add Title text
pres <- ph_with(pres, value = "My first presentation", location = ph_location_type(type = "title"))

# add second slide

pres <- add_slide(pres, layout = "Title and Content", master = "Office Theme")
pres <- ph_with(pres, value = "This is the second slide", location = ph_location_type(type = "title"))
pres <- ph_with(pres, value = c("First line", "Second Line", "Third line"), location = ph_location_type(type = "body"))

##  add a third slide with a table 

# create sample data frame
frame <- data.frame(a = 1:10, b = 11:20, c = 21:30)

# create slide to hold table
pres <- add_slide(pres, layout = "Title and Content", master = "Office Theme")
pres <- ph_with(pres, value = "Table Example", location = ph_location_type(type = "title"))

# add data frame to PowerPoint slide
pres <- ph_with(pres, value = frame, location = ph_location_type(type = "body"))

## Add a slide with a picture

pres <- add_slide(pres)
pres <- ph_with(pres, external_img("lake.jpeg", width = 5, height = 4),
                location = ph_location_type(type = "body"), use_loc_size = FALSE )

# Creating the ppt means printing the object to a file 
print(pres, target = "testPresentation1.pptx")


