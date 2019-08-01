---
title: CICD for CodePipeline
teaser: Setting up a CICD pipeline for a jekyll blog with AWS CodePipeline
tags: jekyll cicd
---

## Intro
I am going to show you how easy it is to set up a simple pipeline that will be able to automatically deploy the latest changes of your jekyll website everytime you push to master.
This ensures that the latest deployed copy of your website is always in sync with the master branch, and simplifies the deployment by controlling it with familiar git tools.

## Pre-requisites
In order to follow along you will need
- A jekyll blog, with the source code stored on github (CodeCommit works too, but no integration for other VCS providers)
- A deployment target for the blog (I am deploying to S3)
- An AWS account

## Deploying the site
In order to deploy my blog I have to create an S3 bucket with website setup. This is extremely simple, just follow these steps
- Create the bucket by running the command `aws s3 mb s3://<my-bucket-name>`
- Setup the website configuration on the bucket: [details here](https://docs.aws.amazon.com/AmazonS3/latest/dev/EnableWebsiteHosting.html)

Now in order to deploy the website, all I have to do is run the following
`jekyll build && aws s3 sync _site s3://<my-bucket-name>`

Unfortunately, this requires that I have the correct AWS credentials whenever I run this command, and I still have to invoke it manually each time there's an update.
So, using AWS CodePipeline I can set this up automatically!

## Automating the deployment
First we need to create a `buildspec.yml` file which will tell the pipeline how to build the blog/website/whatever program you're creating.
By convention, it's easier if you keep this file in the root of your project directory. 

The `buildspec.yml`

```
version: 0.2
phases:
  install:
    commands:
      - gem install jekyll  
  pre_build:
    commands:
      - bundle install
  build:
    commands: 
      - echo "Building site"
      - JEKYLL_ENV=production jekyll build 
      - echo "Copying to S3"
      - aws s3 sync _site s3://$BUCKET_NAME
      - echo "Success"
```

Now, we need to set up AWS CodePipeline and CodeBuild to run this on each push to master.
We do this via CodePipeline. I use CloudFormation to template the pipeline, so I can easily provision (and update/remove) AWS resources. I save the template under `ci/cicd.yml`.
The template can be found on [this website's repo](https://github.com/jhuizy/website/tree/master/ci/cicd.yml).

The template will provision a CodePipeline with a source stage and a build stage. The source stage will trigger based on the parameters of the template. The build stage simply delegates to our buildspec.yml. The bucket it will deploy to is specified as a parameter in the template.

Given the template is located at `ci/cicd.yml`, you can deploy the template as follows:

```
aws cloudformation deploy 
  --stack-name website-cicd \
  --capabilities CAPABILITY_NAMED_IAM \
  --template-file ci/cicd.yml \
  --parameter-overrides \
      GithubBranch=master \
      GithubRepo=<Your repo> \
      GithubOwner=<Your github username> \
      GithubToken=<Your github token> \
      BucketName=<Bucket to deploy to> 

```

Once that has successfully completed, you should be able to push any changes on your blog, and it will automatically deploy to the specified S3 bucket.

## Conclusion
As you can see it's very simple to add CICD for a simple jekyll blog using AWS CodePipeline. Although pretty pointless and probably overkill for this type of project, this can easily be extended to support multiple different types of projects, as most of the work is done in the `buildspec.yml`.
