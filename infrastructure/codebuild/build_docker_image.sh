#!/usr/bin/env bash
set -e
#The source code does not come as a git repository so the tags are lost
#Tag=$(git tag -l)
Tag='latest'
eval $(aws ecr get-login --region=$Region)

# Test to see if repository exists and if not create it
aws ecr describe-images --repository-name $ImageName && repository_exists=True || repository_exists=False
if [ $repository_exists == False ]; then
 echo "$EcrRepository does not exist so we are going to create it"
 aws ecr create-repository --repository-name $ImageName
 echo "Going to build the docker image now"
 docker build --build-arg BASE_URL=$BASE_URL -t $EcrRepository/$ImageName:$Tag .
 echo "pushing the built image to the ecr repository"
 docker push $EcrRepository/$ImageName:$Tag
 echo "exiting script since there is nothing else to do"
 exit 0
fi

echo "$EcrRepository exists so we'll continue building it"

if [ ! -z "$ImageName" ] && [ ! -z "$EcrRepository" ]; then
    if [ ! -z "$Tag" ]; then
        echo "building image as $EcrRepository/$ImageName:$Tag"
        docker pull $EcrRepository/$ImageName:$Tag
	docker build --build-arg BASE_URL=$BASE_URL -t $EcrRepository/$ImageName:$Tag .
    else
        echo "git tag resulted in an empty string so we are just going to label this image as latest"
        docker pull $EcrRepository/$ImageName:latest
	docker build --build-arg BASE_URL=$BASE_URL -t $EcrRepository/$ImageName:latest .
    fi
else
    echo "ImageName and EcrRepository must be set as environment variables to build the docker image"
fi

if [ ! -z "$Region" ]; then
    docker push $EcrRepository/$ImageName:$Tag
else
    echo "Region must be set as an environment variable inorder to push to aws ecr"
fi

