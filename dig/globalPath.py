import os
import arcpy
import sys

work_dir = '../../Workflow_total/1966-2022'
filtered_itp_list_path = os.path.join(work_dir, 'filtered_itp_list')
preparation_path = os.path.join(work_dir, 'prepare_for_cube')
routes_path = os.path.join(preparation_path, 'route_with_id.xlsx')
points_gdb_path = os.path.join(preparation_path, 'points.gdb')
points_gdb_projected_path = os.path.join(preparation_path, 'points_projected.gdb')

cube_path = os.path.join(work_dir, 'cube_nc')
cube_info = os.path.join(cube_path, 'cube_info.txt')


# 创建文件夹或文件地理数据库
def create_path(path: str, path_type: str, out_folder_path: str = None):
    if not os.path.exists(os.path.join(path, out_folder_path) if out_folder_path else path):
        if path_type == "dir":
            os.makedirs(path)
        elif path_type == "gdb" and out_folder_path:
            arcpy.CreateFileGDB_management(out_folder_path=path,
                                           out_name=out_folder_path)
        else:
            raise ValueError("path_type must be 'dir' or 'gdb'")
        print(f'"{path_type}{preparation_path}/{out_folder_path if out_folder_path else ""}" 已创建')
    else:
        print(f'"{path_type}{preparation_path}/{out_folder_path if out_folder_path else ""}" 已存在')

