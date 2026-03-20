# set_color_settings_y2, error messages

    Code
      set_color_settings_y2(dataset = frequencies, color_column = lable)
    Condition
      Error in `set_color_settings_y2()`:
      ! Missing column "lable" in dataset

---

    Code
      set_color_settings_y2(dataset = frequencies, "blue")
    Condition
      Error in `set_color_settings_y2()`:
      ! Not enough colors provided. Please provide 4 more color(s) OR check the "label" column in the data.

---

    Code
      set_color_settings_y2(dataset = frequencies, color_column = group_var, "blue",
        "yellow", "red")
    Condition
      Error in `set_color_settings_y2()`:
      ! Too many colors provided. Please provide 1 less color(s) OR check the "group_var" column in the data.

