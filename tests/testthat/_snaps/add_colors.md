# add_color_palette_y2, color_names to upper case

    Code
      exists(lower_case)
    Condition
      Error:
      ! object 'lower_case' not found

# add_color_palette_y2, error messages

    Code
      add_color_palette_y2(hex_codes = "#123456", color_names = c("color1", "color2"))
    Condition
      Error in `add_color_palette_y2()`:
      ! Check to make sure there are names for every hex code and that all color_names are unique

---

    Code
      add_color_palette_y2(hex_codes = "#123456", color_names = "Y2_BLUE")
    Condition
      Error in `add_color_palette_y2()`:
      ! Custom color name required if custom hex code provided

---

    Code
      add_color_palette_y2(hex_codes = "#41536F", color_names = "color1")
    Condition
      Error in `add_color_palette_y2()`:
      ! Custom hex code required if custom color name provided

---

    Code
      add_color_palette_y2(position = "1")
    Condition
      Error in `add_color_palette_y2()`:
      ! Position must be numeric or a vector of numeric elements

---

    Code
      add_color_palette_y2(position = 0)
    Condition
      Error in `add_color_palette_y2()`:
      ! Position elements must be 1, 2, 3, 4, or 5

