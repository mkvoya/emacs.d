#!/usr/bin/env python3

import requests
import yaml
from datetime import datetime
import pytz
import re

def clean_yaml_content(content):
    # Remove invalid characters
    content = re.sub(r'[^\x00-\x7F]+', '', content)
    return content

def fetch_conference_data():
    url = "https://ccfddl.com/conference/allconf.yml"
    response = requests.get(url)
    if response.status_code == 200:
        # Clean the content before parsing
        cleaned_content = clean_yaml_content(response.text)
        return yaml.safe_load(cleaned_content)
    else:
        raise Exception(f"Failed to fetch data: {response.status_code}")

def get_upcoming_deadlines(conferences, target_conferences=None):
    if target_conferences is None:
        target_conferences = ['SOSP', 'OSDI', 'FAST', 'ASPLOS', 'MICRO', 'ISCA', 'EuroSys', 'USENIX ATC']
    
    current_time = datetime.now(pytz.UTC)
    upcoming_deadlines = []
    
    for conf in conferences:
        if conf['title'] in target_conferences:
            for conf_instance in conf.get('confs', []):
                for timeline in conf_instance.get('timeline', []):
                    if 'deadline' in timeline:
                        deadline_str = timeline['deadline']
                        try:
                            # Parse the deadline
                            deadline = datetime.strptime(deadline_str, '%Y-%m-%d %H:%M:%S')
                            
                            # Handle timezone
                            if 'timezone' in timeline:
                                if timeline['timezone'] == 'UTC-12':
                                    deadline = deadline.replace(tzinfo=pytz.UTC)
                                elif timeline['timezone'] == 'AoE':
                                    # AoE is UTC-12
                                    deadline = deadline.replace(tzinfo=pytz.UTC)
                                else:
                                    # For other timezones, assume UTC
                                    deadline = deadline.replace(tzinfo=pytz.UTC)
                            else:
                                # If no timezone specified, assume UTC
                                deadline = deadline.replace(tzinfo=pytz.UTC)
                            
                            if deadline > current_time:
                                upcoming_deadlines.append({
                                    'conference': conf['title'],
                                    'year': conf_instance['year'],
                                    'deadline': deadline,
                                    'timezone': timeline.get('timezone', 'UTC'),
                                    'comment': timeline.get('comment', '')
                                })
                        except ValueError:
                            print(f"Warning: Could not parse deadline {deadline_str} for {conf['title']} {conf_instance['year']}")
    
    return sorted(upcoming_deadlines, key=lambda x: x['deadline'])

def main():
    try:
        conferences = fetch_conference_data()
        upcoming = get_upcoming_deadlines(conferences)
        
        # Set China timezone
        china_tz = pytz.timezone('Asia/Shanghai')
        
        print("\nUpcoming Conference Deadlines:")
        print("-" * 60)
        for deadline in upcoming:
            # Convert UTC time to China time
            china_time = deadline['deadline'].astimezone(china_tz)
            print(f"{deadline['conference']} {deadline['year']}:")
            print(f"  Deadline: {china_time.strftime('%Y-%m-%d %H:%M:%S')} (China Time)")
            if deadline['comment']:
                print(f"  Note: {deadline['comment']}")
            print()
            
    except Exception as e:
        print(f"Error: {e}")

if __name__ == "__main__":
    main() 