from skillcorner.client import SkillcornerClient
import time
import os

# Replace with your actual credentials
username = "almonadavid@gmail.com"
password = "a6zUDgR3r8UnR5C6"

# Instantiate the client
client = SkillcornerClient(username=username, password=password)

# Alternative: Set environment variables (more secure)
# export SKC_USERNAME="your_email@example.com"
# export SKC_PASSWORD="your_password"
# client = SkillcornerClient()  # Will use environment variables


# Get competitions you have access to for different data types
# Options: 'physical', 'tracking', 'off_ball_runs', 'passes'
data_type = 'tracking'  # Change this based on what you want to download

competitions = client.get_competitions(params={
    'user': 'true', 
    'component_permission_for': data_type
})

print(f"You have access to {len(competitions)} competitions for {data_type} data")
for comp in competitions:
    print(f"- {comp['name']} (ID: {comp['id']})")

# Get Competition Editions(Seasons)
all_competition_editions = []

for competition in competitions:
    competition_id = competition['id']
    competition_editions = client.get_competition_editions(
        competition_id=competition_id, 
        params={'user': 'true', 'component_permission_for': data_type}
    )
    
    # Add competition_id to each edition for reference
    for edition in competition_editions:
        edition['competition_id'] = competition_id
    
    all_competition_editions.extend(competition_editions)

print(f"Total competition editions available: {len(all_competition_editions)}")

# Get Available Matches
all_matches = []

for competition_edition in all_competition_editions:
    competition_edition_id = competition_edition['id']
    matches = client.get_matches(params={
        'competition_edition': competition_edition_id, 
        'limit': 1000
    })
    all_matches.extend(matches)

print(f"Total matches available: {len(all_matches)}")

## Step 4: Mass Download Different Types of Data

### Option A: Download Physical Data
def download_physical_data(matches, output_dir="physical_data"):
    os.makedirs(output_dir, exist_ok=True)
    
    successful_downloads = 0
    failed_downloads = 0
    
    for i, match in enumerate(matches):
        match_id = match['id']
        
        try:
            # Check if data is available
            data_collection = client.get_match_data_collection(match_id=match_id)
            time.sleep(0.1)  # Rate limiting
            
            if data_collection['status'] == "postmatch":
                # Download physical data
                physical_data = client.get_physical(params={
                    'match': match_id, 
                    'data_version': '3'
                })
                
                # Save to file (you'll need to implement your preferred storage method)
                filename = f"{output_dir}/physical_match_{match_id}.json"
                with open(filename, 'w') as f:
                    import json
                    json.dump(physical_data, f)
                
                successful_downloads += 1
                print(f"✓ Downloaded physical data for match {match_id} ({i+1}/{len(matches)})")
            else:
                print(f"✗ Data not available for match {match_id} (status: {data_collection['status']})")
                failed_downloads += 1
                
        except Exception as e:
            print(f"✗ Error downloading match {match_id}: {str(e)}")
            failed_downloads += 1
            
        time.sleep(0.1)  # Rate limiting
    
    print(f"\nDownload Summary:")
    print(f"Successful: {successful_downloads}")
    print(f"Failed: {failed_downloads}")

# Run the download
download_physical_data(all_matches)

### Option B: Download Tracking Data
def download_tracking_data(matches, output_dir="tracking_data"):
    os.makedirs(output_dir, exist_ok=True)
    
    for i, match in enumerate(matches):
        match_id = match['id']
        
        try:
            # Check if data is available
            data_collection = client.get_match_data_collection(match_id=match_id)
            time.sleep(0.1)
            
            if data_collection['status'] == "postmatch":
                # Download tracking data
                filepath = f"{output_dir}/tracking_match_{match_id}.json"
                client.save_match_tracking_data(
                    match_id=match_id, 
                    filepath=filepath,
                    params={'data_version': '3'}
                )
                print(f"✓ Downloaded tracking data for match {match_id} ({i+1}/{len(matches)})")
            else:
                print(f"✗ Tracking data not available for match {match_id}")
                
        except Exception as e:
            print(f"✗ Error downloading tracking for match {match_id}: {str(e)}")
            
        time.sleep(0.1)  # Rate limiting

# Run the download
download_tracking_data(all_matches)

### Option C: Download Dynamic Events

def download_dynamic_events(matches, output_dir="dynamic_events"):
    os.makedirs(output_dir, exist_ok=True)
    
    for i, match in enumerate(matches):
        match_id = match['id']
        
        try:
            # Check if dynamic events are available
            data_collection = client.get_match_data_collection(match_id=match_id)
            time.sleep(0.1)
            
            if data_collection.get('dynamic_events_check', False):
                # Download CSV format
                csv_filepath = f"{output_dir}/dynamic_events_{match_id}.csv"
                client.save_dynamic_events(
                    match_id, 
                    params={'file_format': 'csv'},
                    filepath=csv_filepath
                )
                
                # Optionally download XML format
                xml_filepath = f"{output_dir}/dynamic_events_{match_id}.xml"
                client.save_dynamic_events(
                    match_id,
                    params={'file_format': 'sportscode-xml'},
                    filepath=xml_filepath
                )
                
                print(f"✓ Downloaded dynamic events for match {match_id} ({i+1}/{len(matches)})")
            else:
                print(f"✗ Dynamic events not available for match {match_id}")
                
        except Exception as e:
            print(f"✗ Error downloading dynamic events for match {match_id}: {str(e)}")
            
        time.sleep(0.1)  # Rate limiting

# Run the download
download_dynamic_events(all_matches)

## Step 5: Complete Mass Download Script


def mass_download_all_data():
    """Download all available data types for all accessible matches"""
    
    print("Starting mass download process...")
    
    # Get all your accessible data
    competitions = client.get_competitions(params={'user': 'true'})
    all_matches = []
    
    for competition in competitions:
        competition_id = competition['id']
        competition_editions = client.get_competition_editions(competition_id=competition_id)
        
        for edition in competition_editions:
            matches = client.get_matches(params={
                'competition_edition': edition['id'], 
                'limit': 1000
            })
            all_matches.extend(matches)
    
    print(f"Found {len(all_matches)} total matches")
    
    # Create output directories
    os.makedirs("downloads/physical", exist_ok=True)
    os.makedirs("downloads/tracking", exist_ok=True)
    os.makedirs("downloads/dynamic_events", exist_ok=True)
    os.makedirs("downloads/metadata", exist_ok=True)
    
    # Download all data types
    for i, match in enumerate(all_matches):
        match_id = match['id']
        print(f"\nProcessing match {match_id} ({i+1}/{len(all_matches)})")
        
        try:
            # Get match metadata and data collection status
            data_collection = client.get_match_data_collection(match_id=match_id)
            match_metadata = client.get_match(match_id=match_id)
            
            # Save metadata
            with open(f"downloads/metadata/match_{match_id}_metadata.json", 'w') as f:
                import json
                json.dump({
                    'match_data': match_metadata,
                    'data_collection': data_collection
                }, f, indent=2)
            
            # Download different data types if available
            if data_collection['status'] == "postmatch":
                
                # Physical data
                try:
                    physical_data = client.get_physical(params={'match': match_id, 'data_version': '3'})
                    with open(f"downloads/physical/match_{match_id}_physical.json", 'w') as f:
                        json.dump(physical_data, f)
                    print("  ✓ Physical data downloaded")
                except:
                    print("  ✗ Physical data failed")
                
                # Tracking data
                try:
                    client.save_match_tracking_data(
                        match_id=match_id,
                        filepath=f"downloads/tracking/match_{match_id}_tracking.json",
                        params={'data_version': '3'}
                    )
                    print("  ✓ Tracking data downloaded")
                except:
                    print("  ✗ Tracking data failed")
                
                # Dynamic events
                if data_collection.get('dynamic_events_check', False):
                    try:
                        client.save_dynamic_events(
                            match_id,
                            params={'file_format': 'csv'},
                            filepath=f"downloads/dynamic_events/match_{match_id}_events.csv"
                        )
                        print("  ✓ Dynamic events downloaded")
                    except:
                        print("  ✗ Dynamic events failed")
            
        except Exception as e:
            print(f"  ✗ Error processing match {match_id}: {str(e)}")
        
        time.sleep(0.2)  # Rate limiting
    
    print("\nMass download completed!")

# Run the complete download
mass_download_all_data()


## Step 6: Additional Tips

### Filtering and Incremental Downloads
# Download only matches from a specific date range
from datetime import datetime

recent_matches = client.get_matches(params={
    'physical_last_modified__gte': '2024-01-01',  # Only matches updated since Jan 1, 2024
    'limit': 1000
})

# Download only specific competition
premier_league_matches = client.get_matches(params={
    'competition': 4,  # Replace with actual competition ID
    'limit': 1000
})

# Download only specific team's matches
team_matches = client.get_matches(params={
    'team_id': 123,  # Replace with actual team ID
    'limit': 1000
})

### Error Handling and Logging
import logging

# Set up logging
logging.basicConfig(
    filename='skillcorner_download.log',
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)

def safe_download_with_retry(download_func, max_retries=3):
    """Wrapper function to retry downloads on failure"""
    for attempt in range(max_retries):
        try:
            return download_func()
        except Exception as e:
            logging.warning(f"Attempt {attempt + 1} failed: {str(e)}")
            if attempt == max_retries - 1:
                logging.error(f"All {max_retries} attempts failed")
                raise e
            time.sleep(2 ** attempt)  # Exponential backoff