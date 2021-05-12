ARG MUGGLE_TAG=0.1.1-20210308
FROM subugoe/muggle-buildtime-onbuild:${MUGGLE_TAG} as buildtime
CMD 1+1
