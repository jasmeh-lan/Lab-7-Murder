Jasmine Lantaca

For this lab, you will be joining and filtering related data sets to
solve a murder mystery!

# Part 1: GitHub Workflow

At the top of the document (in the YAML) there is an `author` line that
says `"Your name here!"`. Change this to be your name and save your file
either by clicking on the blue floppy disk or with a shortcut (command /
control + s).

Be sure to [commit the files to your
repo](https://happygitwithr.com/existing-github-first#stage-and-commit).

Let’s get started!

# Part 2: Some Words of Advice

- Set chunk options carefully.

- Make sure you don’t print out more output than you need.

- Make sure you don’t assign more objects than necessary—avoid “object
  junk” in your environment.

- Make your code readable and nicely formatted.

- Think through your desired result **before** writing any code.

# Part 3: Finding the Killer

Northwestern University’s Knight Lab wanted to help sharpen users’
database skills, so they created a murder mystery. Can you solve this
crime in SQL City??

The relational data you will be working with contains tables with
different pieces of information pertinent to the crime - people, social
media check-ins, driver’s licenses, crime scene reports, police
interviews, and more!

## Access the Data

This code chunk will read in **all** of the tables of data for you.
Don’t modify or remove this! Take some time to look at each file type so
that

``` r
library(tidyverse)

# If purrr is not detected, install the package
if (!"purrr" %in% installed.packages()) install.packages("purrr")

source("https://raw.githubusercontent.com/jcanner/stat_210_2025_website/main/labs/instructions/lab-7-setup.R")
```

## Solve the Crime

### Crime Scene Report

Detective Wickham reaches out to you…

> A crime has taken place and I need your help! There was a murder in
> SQL City sometime on January 15, 2018. Could you retrieve the crime
> scene report from the police department’s database and follow the
> clues to find the person responsible for the murder?!

**Step 1: Find the police report in `crime_scene_report`. Then used the
information in the report to move on to the next data source based on
the information learned.**

``` r
# Code for looking at the relevant crime scene report.

crime_scene_report |>
  filter(date == 20180115 & type == "murder" & city == "SQL City") |>
  select(description) # trying to just get the description of the witnesses
```

    # A tibble: 1 × 1
      description                                                                   
      <chr>                                                                         
    1 "Security footage shows that there were 2 witnesses. The first witness lives …

``` r
Annabel <- person |>
  filter(address_street_name == "Franklin Ave" & str_detect(name, "Annabel")) |> # Had to use ChatGPT to figure out how to get the code that lets me contain strings or numbers 
  left_join(interview, by = c("id" = "person_id")) |>
  select(transcript)

Annabel
```

    # A tibble: 1 × 1
      transcript                                                                    
      <chr>                                                                         
    1 I saw the murder happen, and I recognized the killer from my gym when I was w…

``` r
Witness_1 <- person |> 
  filter(address_street_name == "Northwestern Dr") |>
  arrange(desc(address_number)) |>
  slice(1) |> # found Mr. Morty Schapiro
  left_join(interview, by = c("id" = "person_id")) |>
  select(transcript)

Witness_1
```

    # A tibble: 1 × 1
      transcript                                                                    
      <chr>                                                                         
    1 "I heard a gunshot and then saw a man run out. He had a \"Get Fit Now Gym\" b…

``` r
Report_2 <- get_fit_now_check_in |>
  filter(check_in_date == 20180109) |>
  left_join(get_fit_now_member, by = c("membership_id" = "id"))

Report_2
```

    # A tibble: 10 × 8
       membership_id check_in_date check_in_time check_out_time person_id name      
       <chr>                 <dbl>         <dbl>          <dbl>     <dbl> <chr>     
     1 X0643              20180109           957           1164     15247 Shondra L…
     2 UK1F2              20180109           344            518     28073 Zackary C…
     3 XTE42              20180109           486           1124     55662 Sarita Ba…
     4 1AE2H              20180109           461            944     10815 Adriane P…
     5 6LSTG              20180109           399            515     83186 Burton Gr…
     6 7MWHJ              20180109           273            885     31523 Blossom C…
     7 GE5Q8              20180109           367            959     92736 Carmen Di…
     8 48Z7A              20180109          1600           1730     28819 Joe Germu…
     9 48Z55              20180109          1530           1700     67318 Jeremy Bo…
    10 90081              20180109          1600           1700     16371 Annabel M…
    # ℹ 2 more variables: membership_start_date <dbl>, membership_status <chr>

``` r
Report_1 <- get_fit_now_member |>
  filter(membership_status == "gold" & str_detect(id, "48Z")) |>
  select(person_id, name) |>
  left_join(person, by = c("person_id" = "id")) |> # I need to find the license ID of the person before joining with driver license data
  select(name.x, license_id) |>
  left_join(drivers_license, by = c("license_id" = "id")) |>
  filter(str_detect(plate_number, "H42W")) |>
  inner_join(Report_2, by = c("name.x" = "name")) |> #Joining the information from report 2 to report 1 to see if that is truely the person
  select(name.x, person_id)

Report_1
```

    # A tibble: 1 × 2
      name.x        person_id
      <chr>             <dbl>
    1 Jeremy Bowers     67318

``` r
Murder_Check <- interview |>
  right_join(Report_1, by = "person_id") |>
  select(transcript)
Murder_Check # They were a hitman but not the true culprit
```

    # A tibble: 1 × 1
      transcript                                                                    
      <chr>                                                                         
    1 "I was hired by a woman with a lot of money. I don't know her name but I know…

``` r
Woman <- drivers_license |>
  filter(hair_color == "red" & car_make == "Tesla" & car_model == "Model S") |>
  filter(!height == 69) |> # removing the person who was too tall in the group of people
  left_join(person, by = c("id" = "license_id")) |>
  select(id, id.y, name) |>
  left_join(interview, by = c("id.y" = "person_id")) |> # No interviews from each suspect
  left_join(facebook_event_checkin, by = c("id.y" = "person_id")) ## found the suspect (Only one person of the three went to the concert three times)

Woman
```

    # A tibble: 5 × 7
          id  id.y name             transcript event_id event_name              date
       <dbl> <dbl> <chr>            <chr>         <dbl> <chr>                  <dbl>
    1 202298 99716 Miranda Priestly <NA>           1143 SQL Symphony Concert  2.02e7
    2 202298 99716 Miranda Priestly <NA>           1143 SQL Symphony Concert  2.02e7
    3 202298 99716 Miranda Priestly <NA>           1143 SQL Symphony Concert  2.02e7
    4 291182 90700 Regina George    <NA>             NA <NA>                 NA     
    5 918773 78881 Red Korb         <NA>             NA <NA>                 NA     

**Next Steps: Follow the evidence to the person responsible for the
murder, building a report as you go.** There are accomplices, some
knowingly and some unknowingly, but there is only one mastermind.

Solve the murder mystery, showing **all of your work in this document**.
Your document and code must be well organized, easy to follow, and
reproducible.

- Use headers and written descriptions to indicate what you are doing.
- Use `dplyr` verbs and `join` functions rather than just looking
  through the tables manually. Functions from `stringr` and `lubridate`
  will also be useful.
- Use good code formatting practices.
- Comment your code.
- Cite any external sources you use to solve the mystery.

> [!NOTE]
>
> ### And the final suspect is…
>
> *Miranda Priestly*
