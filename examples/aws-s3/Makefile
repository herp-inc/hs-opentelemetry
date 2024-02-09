.PHONY: prepare.bucket
prepare.bucket:
	aws --endpoint-url=http://localhost:4566 s3api create-bucket --bucket aws-s3-example
	aws --endpoint-url=http://localhost:4566 s3api put-object --bucket aws-s3-example --key README.md --body README.md
