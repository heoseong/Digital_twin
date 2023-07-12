## List of data downloaded from JDC
#You may need to install the Korean language pack

1. 제주국제자유도시개발센터_JDC 농가설비정보_20211224.xlsx = 1. citrus_farm_equipment_information.csv
2. 제주국제자유도시개발센터_JDC농가기본정보_20211224.xlsx = 2. citrus_farm.csv
3. 제주국제자유도시개발센터_JDC농가환경데이터_20211224.xlsx = 3. citrus_environment2.csv
4. 제주국제자유도시개발센터_JDC영농행위정보_20211021.xlsx = 4. citrus_practice.csv
5. 제주국제자유도시개발센터_JDC위치과실당도데이터_20211224.xlsx = 5. citrus_fruit_data_3_replicate.csv
6. 제주국제자유도시개발센터_JDC주별과실당도데이터_20211201.xlsx = 6. citrus_fruit_data_by_tree.csv


## Merge data
7. raw_data.csv = citrus_farm.csv + citrus_farm_equipment_information.csv

8. soil_selected_farm.csv : manually selected citrus farm based on soil_data_for_citrus_farm_list_integration.csv file and soil_data_for_orchard.csv
Refer to 09_data_merging.R file
 
9. practice_groupby.csv: Refer to 09_data_merging.R file

10. for_real_machine_learning.csv: Manually edited
