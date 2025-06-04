#!/usr/bin/env python3
import os
import gitlab
import yaml
from datetime import datetime, timedelta
from collections import defaultdict
from concurrent.futures import ThreadPoolExecutor

class GitLabMonitor:
    def __init__(self, config):
        # 从配置获取 GitLab 配置
        self.gitlab_url = config['gitlab']['url']
        self.gitlab_token = config['gitlab']['token']
        
        if not self.gitlab_url or not self.gitlab_token:
            raise ValueError("请在 config.yaml 中设置 gitlab.url 和 gitlab.token")
        
        # 初始化 GitLab 客户端
        self.gl = gitlab.Gitlab(url=self.gitlab_url, private_token=self.gitlab_token)
    
    def get_user_commits(self, username, days=3):
        """
        获取指定用户在最近几天内的提交记录
        
        Args:
            username (str): GitLab 用户名
            days (int): 要查询的天数范围
        
        Returns:
            dict: 按天分组的提交记录字典
        """
        try:
            # 获取用户信息
            user = self.gl.users.list(username=username)[0]
            
            # 计算时间范围
            since_date = datetime.now().date() - timedelta(days=3)
            
            # 使用 defaultdict 按天分组存储提交记录
            commits_by_day = defaultdict(lambda: {
                'commits': [],
                'additions': 0,
                'deletions': 0,
                'projects': set()
            })
            
            # 获取用户的活动记录
            events = self.gl.users.get(user.id).events.list(get_all=True, after=since_date.isoformat())
            
            for event in events:
                # 只处理推送事件
                if "push" in event.action_name:
                    try:
                        # 获取项目信息
                        project_id = event.project_id
                        project = self.gl.projects.get(project_id)
                        
                        # 获取推送事件中的提交信息
                        push_data = event.push_data
                        commit_to = push_data.get('commit_to', '')
                        commit_count = push_data.get('commit_count', 0)
                        commit = project.commits.get(commit_to)
                        # print(commit_count)

                        for _ in range(commit_count):
                            # 将提交时间转换为日期（去掉时间部分）
                            commit_date = datetime.fromisoformat(commit.created_at).date()
                            # print(commit_date)
                            if commit_date < datetime.now().date() - timedelta(days=3):
                                break
                            
                            # 获取提交的详细信息，包括文件变更
                            commit_detail = project.commits.get(commit.id)
                            
                            commits_by_day[commit_date]['commits'].append(commit.id)
                            commits_by_day[commit_date]['additions'] += commit_detail.stats.get('additions', 0)
                            commits_by_day[commit_date]['deletions'] += commit_detail.stats.get('deletions', 0)
                            commits_by_day[commit_date]['projects'].add(project.name)
                            commit = project.commits.get(commit.parent_ids[0])

                    except Exception as e:
                        print(f"处理事件时出错: {str(e)}")
            
            return commits_by_day
        
        except Exception as e:
            print(f"获取用户 {username} 的提交记录时出错: {str(e)}")
            return defaultdict(lambda: {
                'commits': [],
                'additions': 0,
                'deletions': 0,
                'projects': set()
            })

def format_date(date):
    """格式化日期显示"""
    today = datetime.now().date()
    yesterday = today - timedelta(days=1)
    day_before_yesterday = today - timedelta(days=2)
    
    if date == today:
        return "今天"
    elif date == yesterday:
        return "昨天"
    elif date == day_before_yesterday:
        return "前天"
    else:
        return date.strftime("%Y-%m-%d")

def load_config():
    """加载配置文件"""
    config_path = os.path.join(os.path.dirname(__file__), 'config.yaml')
    if not os.path.exists(config_path):
        with open(config_path, 'w') as f:
            f.write("""# GitLab 配置
gitlab:
  url: https://your-gitlab-instance.com
  token: your-personal-access-token

# 要监控的用户列表
users:
  - user1
  - user2
""")
        print("已创建 config.yaml 文件模板，请填写你的 GitLab 配置信息")
        return None
    
    with open(config_path, 'r') as f:
        return yaml.safe_load(f)

def main():
    # 加载配置
    config = load_config()
    if not config:
        return

    try:
        monitor = GitLabMonitor(config)
        
        # 获取要监控的用户列表
        users_to_monitor = config.get('users', [])
        if not users_to_monitor:
            print("请在 config.yaml 中设置 users 列表")
            return
        
        summary = {}
        # 按顺序执行以保证打印顺序
        # 创建线程池
        with ThreadPoolExecutor(max_workers=10) as executor:
            def process_user(username):
                commits_by_day = monitor.get_user_commits(username)
                summary[username] = commits_by_day
            executor.map(process_user, users_to_monitor)

        print("\nRecent Gitlab Activities:")
        print("-" * 60)
        for username in users_to_monitor:
            print(f"\n{username}:")
            if summary[username]:
                # 按日期排序（从新到旧）
                sorted_dates = sorted(summary[username].keys(), reverse=True)
                
                for date in sorted_dates:
                    day_stats = summary[username][date]
                    projects_list = ", ".join(sorted(day_stats['projects']))
                    print(f"{format_date(date)}: {len(day_stats['commits'])} commits, {day_stats['additions']}+ {day_stats['deletions']}-, proj: {projects_list}")
            else:
                print("未找到提交记录")

    except Exception as e:
        print(f"程序运行出错: {str(e)}")

if __name__ == "__main__":
    main() 