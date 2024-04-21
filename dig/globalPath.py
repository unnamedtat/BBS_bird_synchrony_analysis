import os

filtered_itp_list_path = '../../Workflow_total/1966-2022/filtered_itp_list'
save_path = '../../Workflow_total/1966-2022/prepare_for_cube'
routes_path = os.path.join(save_path, 'route_with_id.xlsx')
points_gdb_path = os.path.join(save_path, 'points.gdb')
points_gdb_projected_path = os.path.join(save_path, 'points_projected.gdb')