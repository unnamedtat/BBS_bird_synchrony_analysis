import arcpy
from globalPath import *

if __name__ == '__main__':
    try:
        arcpy.env.overwriteOutput = True
        arcpy.env.workspace = cube_analysis_gdb
    except arcpy.ExecuteError:
        print(arcpy.GetMessages(2))
