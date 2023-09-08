library(tidyverse)
library(ggthemr)

ggthemr('flat')

# data(mpg)

ggplot2::mpg

### Question #1

# cyl
# hwy
# 
# ggplot(data = mpg, aes(x = cyl, y = hwy) + 
#   geom_point() +
#   geom_smooth(method = lm, se = FALSE)
# 
# geom_smooth(mapping = aes (x = displ, y = hwy, linetype = as.factor (cyl)))

# mpg <- mpg %>%
#         mutate(cyl = as.factor(cyl))

ggplot(data = mpg) + 
  geom_boxplot(aes(x = as.factor(cyl), y = hwy, color = as.factor(cyl)), fill = NA)

### Question #2

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_point() + 
  geom_abline() +
  theme(aspect.ratio = .75)

+
  coord_fixed()


### Question #3