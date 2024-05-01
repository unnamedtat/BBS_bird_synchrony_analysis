import arcpy
from globalPath import *

def buildCube():
    arcpy.env.workspace = cube_path
    walk = arcpy.da.Walk(points_gdb_projected_path, datatype="FeatureClass")
    for dirpath, dirnames, filenames in walk:
        for filename in filenames:
            p_path = os.path.join(dirpath, filename)
            cube = arcpy.stpm.CreateSpaceTimeCube(in_features=p_path, output_cube=f"{filename}.nc",
                                                  time_field="year_Converted", time_step_interval="1 Years",
                                                  time_step_alignment="End_time",
                                                  distance_interval="1000 Kilometers",
                                                  summary_fields="sum SUM SPACE_TIME_NEIGHBORS",
                                                  aggregation_shape_type="FISHNET_GRID")
            # FISHNET_GRID—输入要素将要聚合到方形（渔网）像元网格中。
            # HEXAGON_GRID—输入要素将要聚合到六边形像元网格中。
            # DEFINED_LOCATIONS—输入要素将聚合到所提供的位置。

if __name__ == '__main__':
    create_path(cube_path, 'dir')
    logfile = open(cube_info, 'w')
    logAddMessage = ArcgisLogger(logfile).logAddMessage
    arcpy.AddMessage = logAddMessage
    try:
        arcpy.env.overwriteOutput = True
        buildCube()
    except arcpy.ExecuteError:
        print(arcpy.GetMessages(2))
    logfile.close()
