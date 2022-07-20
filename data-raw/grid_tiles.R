# x and y cols constructed by me through trial and error
# res data (meters per pixel) from OS API Technical Spec.
grid_tiles_27700 <- dplyr::tribble(
  ~zoom, ~x,    ~y,   ~res,
  0,      4,     6,   896.0,
  1,      9,    12,   448.0,
  2,     19,    24,   224.0,
  3,     38,    48,   112.0,
  4,     79,    96,    56.0,
  5,    158,   197,    28.0,
  6,    317,   395,    14.0,
  7,    635,   791,     7.0,
  8,   1270,  1582,     3.5,
  9,   2541,  3164,     1.75,
  10,  5082,  6328,     0.875,
  11, 10164, 12657,     0.4375,
  12, 20328, 25314,     0.21875,
  13, 40656, 50628,     0.109375
) %>%
  dplyr::mutate(across(x:y, ~ `+`(., 1)))

# grid_tiles_3857 <- dplyr::tribble(
#   ~zoom, ~x, ~y, ~xmin, ~xmax, ~ymin, ~ymax,
#   7,      5,     8,     60,     64,     36,     43,
#   8,      9,    17,    120,    129,     71,     87,
#   9,     21,    32,    240,    260,    143,    174,
#   10,    38,    64,    481,    518,    286,    349,
#   11,    75,   126,    962,   1036,    573,    698,
#   12,   148,   252,   1925,   2072,   1146,   1397,
#   13,   294,   503,   3851,   4144,   2292,   2794,
#   14,   588,  1006,   7702,   8289,   4584,   5589,
#   15,  1176,  2011,  15404,  16579,   9169,  11179,
#   16,  2351,  4022,  30808,  33158,  18338,  22359,
#   17,  4701,  7646,  61616,  66316,  37073,  44718,
#   18,  9401, 16084, 123233, 132633,  73353,  89436,
#   19, 18800, 32167, 246467, 265266, 146706, 178872,
#   20, 37598, 64334, 492935, 530532, 293412, 357745
# )


usethis::use_data(grid_tiles_27700, overwrite = TRUE, internal = TRUE)

# (0,0) (SW origin in 27700) is (-841259.2, 6405988) in 3857
# (7e5, 1.3e6) (NE corner in 27700) is (404617.9, 8733429) in 3857
# (3.5e5, 6.5e5) centre point of UK? is (-311463.7, 7506986) in 3857
# 8733429 - 6405988 = 2327441
# 404617.9 - -841259.2 = 1245877

# length of equator in WGS-84 is 40075016.686m

# 3857:
# furthest W line is -20026376.39 (metres west of Greenwich)
# furthest E line is 20026376.39 (metres east of Greenwich)
# furthest N line is 20048966.10 (metres north of equator)
# furthest S line is -20048966.10 (metres south of equator)

# at zoom level 7 the earth is divided into 2^7 x 2^7 (2^14 = 16384) tiles
# the western edge of the UK at -841259.2 is equivalent to
# 20026376.39 + -841259.2 = 19185117m
# 19185117 / (2*20026376.4) = 0.4789962
# 19185117 / 40075017 = 0.4787301
# the eastern edge of the UK at 404617.9 is equivalent to
# 20026376.39 + 404617.9 = 20430994m
# 20430994 / (2*20026376.4) = 0.5101021
# 20430994 / 40075017 = 0.5098187
# the northern edge of the UK at 8733429 (m north of equator) is
# 20048966.10 - 8733429 = 11315537m from northern boundary line
# 11315537 / (2 * 20048966.10) = 0.2821975
# the southern edge of the UK at 6405988 (m north of equator) is
# 20048966.10 - 6405988 = 13642978 from northern boundary line
# 13642978 / (2 * 20048966.10) = 0.3402414

# the north-westernmost tile in the UK should be (at zoom 7):
# north:
# 0.2821975 * 2^7
# = 36.12128
# west:
# 0.4789962 * 2^7
# 0.4787301 * 2^7
# = 61.31151
# = 61.27745
# in fact (OS API): x = 60, y = 36

# centrepoint:
# x (west-east):
# (20026376.4 + -311463.7) * 2^7 / 40052752.8
# = 63.00463
# y (north-south):
# (20048966.10 - 7506986) * 2^7 / (2 * 20048966.10)
# = 40.03632
# (40, 63) gives us a tile around the Newcastle-upon-Tyne area
