import os
import hashlib

def calculate_md5(file_path):
    """计算文件的MD5值"""
    hasher = hashlib.md5()
    with open(file_path, 'rb') as file:
        for chunk in iter(lambda: file.read(4096), b''):
            hasher.update(chunk)
    return hasher.hexdigest()

def generate_md5_file(folder_path):
    """在文件夹内生成MD5文件"""
    md5_file_path = os.path.join(folder_path, 'md5.txt')
    with open(md5_file_path, 'w') as md5_file:
        # 遍历文件夹内的文件
        for root, _, files in os.walk(folder_path):
            for file in files:
                filename = os.path.join(root, file)
                # 计算文件的MD5值并写入MD5文件
                md5 = calculate_md5(filename)
                md5_file.write(f"{md5}  ./{file}\n")

# 获取当前路径
current_path = os.getcwd()

# 遍历当前路径下的所有文件夹
for folder in os.listdir(current_path):
    folder_path = os.path.join(current_path, folder)
    if os.path.isdir(folder_path):
        generate_md5_file(folder_path)