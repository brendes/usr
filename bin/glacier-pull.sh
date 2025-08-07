#!/bin/bash

BUCKET="$1"
KEY="$2"
DESTINATION="./$2"

while true; do
    STATUS=$(aws s3api head-object --bucket "$BUCKET" --key "$KEY" --query "Restore" --output text)
    echo $STATUS
    if [ "$STATUS" != "None" ] && [[ "$STATUS" == *"ongoing-request=\"false\""* ]]; then
        echo "Restoration complete. Copying file..."
        aws s3 cp "s3://$BUCKET/$KEY" "$DESTINATION"
        echo "File copied to $DESTINATION"
        break
    else
        echo "Restoration in progress..."
    fi

    # Wait for a specified amount of time before checking the status again
    sleep 300  # Adjust the sleep time as needed
done

