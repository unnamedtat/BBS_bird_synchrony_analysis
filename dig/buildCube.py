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
                                                  distance_interval="250 Kilometers",
                                                  summary_fields="sum SUM SPACE_TIME_NEIGHBORS",
                                                  aggregation_shape_type="HEXAGON_GRID")

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
