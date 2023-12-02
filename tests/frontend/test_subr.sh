new_pkg() {
	cat << EOF > $1.ucl
name: $2
namebase: $2
subpackage: $3
variant: $4
version: "$5"
maintainer: test
categories: [test]
comment: a test
www: http://test
prefix: ${6}
abi: "*"
desc: <<EOD
This is a test
EOD
EOF
}

SUBCMD=$1
shift
${SUBCMD} $*
