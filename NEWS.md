# mesonet 0.0.2.9001

- Bugfixes to mnet_requisition_list() including:
    - Assigning time zone properly for start_date and end_date when specified
      as Date objects
    - Adjusting end_date forward to the following day for character and Date
      inputs to account for UTC/CST time zone offset

# mesonet 0.0.2

- Bugfixes including:
    - Updating calls to units::set_units() based on new argument name
    - Changing behavior when the Mesonet website is unavailable to
      "fail gracefully with an informative message" in accordance with 
      CRAN policy

# mesonet 0.0.1

- Release of first complete version of package!

- This version provides functions that can:
    - download Oklahoma Mesonet station metadata
    - download Oklahoma Mesonet time series (MTS) files
    - import MTS files into R
    - create a local cache of downloaded Oklahoma Mesonet data
    - convert soil temperature change (delta T) measurements into matric potential
    - convert matric potential into volumetric soil moisture
