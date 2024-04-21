import pandas as pd
import os
import arcpy

arcpy.env.overwriteOutput = True
arcpy.env.workspace = os.getenv("TEMP")
filtered_itp_list_path = '../../Workflow_total/1966-2022/filtered_itp_list'
save_path = '../../Workflow_total/1966-2022/prepare_for_cube'
routes_path = os.path.join(save_path,'route_with_id.xlsx')

out_gdb_path = os.path.join(save_path, 'points.gdb')
if not os.path.exists(out_gdb_path):
    arcpy.CreateFileGDB_management(out_folder_path=save_path, out_name='points.gdb')
    print(f'数据库 "{out_gdb_path}" 已创建')
else:
    print(f'数据库 "{out_gdb_path}" 已存在')

routes = pd.read_excel(routes_path, sheet_name=0, header=0)

if not os.path.exists(save_path):
    os.makedirs(save_path)
    print(f'目录 "{save_path}" 已创建')
else:
    print(f'目录 "{save_path}" 已存在')

# 遍历文件夹中的所有xlsx文件
for file_name in os.listdir(filtered_itp_list_path):
    if file_name.endswith('.xlsx'):
        file_path = os.path.join(filtered_itp_list_path, file_name)

        df = pd.read_excel(file_path, sheet_name=0, header=0)

        new_df = pd.DataFrame(columns=['sum', 'RouteID', 'year'])

        for col in range(1, 58):
            # 将原始数据框中的列值放入新的sum列
            temp_df = pd.DataFrame({'sum': df[f'{col}'].values,
                                    'RouteID': df['names(x)'].values, 'year': col + 1965})
            new_df = pd.concat([new_df, temp_df], ignore_index=True, axis=0)
        # 保存为新的Excel文件
        # new_file_name = os.path.splitext(file_name)[0] + '.xlsx'
        # new_file_path = os.path.join(save_path, new_file_name)
        # new_df.to_excel(new_file_path, index=False, header=True)
        merged_df = new_df.merge(routes, how='left', on='RouteID')

        out_shp_name = "AOU" + os.path.splitext(file_name)[0]
        out_shp_path = os.path.join(out_gdb_path, out_shp_name)
        sr = arcpy.SpatialReference(4326)
        arcpy.CreateFeatureclass_management(out_path=out_gdb_path, out_name=out_shp_name,
                                            geometry_type="POINT", spatial_reference=sr)
        # 添加字段
        arcpy.AddField_management(out_shp_path, "Latitude", "DOUBLE")
        arcpy.AddField_management(out_shp_path, "Longitude", "DOUBLE")
        arcpy.AddField_management(out_shp_path, "RouteID", "LONG")
        arcpy.AddField_management(out_shp_path, "sum", "LONG")
        arcpy.AddField_management(out_shp_path, "year", "LONG")
        # 使用InsertCursor将数据写入
        fields = ["SHAPE@XY", "Latitude", "Longitude", "RouteID", "sum", "year"]
        with arcpy.da.InsertCursor(out_shp_path, fields) as cursor:
            for index, row in merged_df.iterrows():
                lat = row["Latitude"]
                lon = row["Longitude"]
                routeid = row["RouteID"]
                sum_value = int(row["sum"])
                year_value = int(row["year"])
                cursor.insertRow([(lon, lat), lat, lon, routeid, sum_value, year_value])

        print(f"Created feature class: {out_shp_path}")

        # 投影到所需坐标系 (以下为北美地区常用投影坐标系)
        projected_shp_name = out_shp_name + "_projected"
        projected_shp_path = os.path.join(out_gdb_path, projected_shp_name)
        out_coord_system = arcpy.SpatialReference(102039) # USA_Contiguous_Albers_Equal_Area_Conic
        # out_coord_system = arcpy.SpatialReference(102008)  # North_America_Equidistant_Conic
        arcpy.Project_management(in_dataset=out_shp_path,
                                 out_dataset=projected_shp_path,
                                 out_coor_system=out_coord_system)
        print(f"Created feature class: {projected_shp_path}")

