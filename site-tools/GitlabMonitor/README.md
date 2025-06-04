# GitLab 提交监控工具

这是一个用于监控 GitLab 用户提交情况的 Python 脚本。

## 功能特点

- 通过用户活动 API 高效获取 GitLab 提交记录
- 按天分组显示最近三天的提交记录
- 显示详细的提交信息，包括项目名称、提交ID和标题
- 使用友好的日期显示（今天、昨天、前天）

## 安装依赖

```bash
pip install -r requirements.txt
```

## 配置

1. 在 GitLab 中创建个人访问令牌（Personal Access Token）
   - 登录 GitLab
   - 进入 Settings > Access Tokens
   - 创建一个新的访问令牌，确保勾选 `read_api` 权限

2. 配置 config.yaml 文件
   - 复制 `config.yaml` 文件模板（如果不存在会自动创建）
   - 填写以下信息：
     ```yaml
     # GitLab 配置
     gitlab:
       url: https://your-gitlab-instance.com
       token: your-personal-access-token

     # 要监控的用户列表
     users:
       - user1
       - user2
     ```

## 使用方法

1. 在 `config.yaml` 文件中设置要监控的用户列表：
   ```yaml
   users:
     - user1
     - user2
     - user3
   ```

2. 运行脚本：
   ```bash
   python gitlab_monitor.py
   ```

## 输出示例

```
获取用户 user1 的提交记录:

今天 (2 条提交):
  - 项目: project-name
    提交: Update README
    ID: abc123

昨天 (1 条提交):
  - 项目: another-project
    提交: Fix bug in login
    ID: def456
```

## 注意事项

- 确保有足够的权限访问要监控的用户活动
- 访问令牌请妥善保管，不要提交到版本控制系统中
- 默认只显示最近 3 天的提交记录
- 用户列表中的用户名必须是 GitLab 上的有效用户名
- 脚本使用 GitLab 的用户活动 API，比遍历所有项目更高效 