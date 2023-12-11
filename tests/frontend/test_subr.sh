new_pkg() {
	cat << EOF > $1.ucl
namebase: "$2"
subpackage: "$3"
variant: "$4"
version: "$5"
maintainer: "test"
categories: ["test"]
comment: "a test"
www: "http://test"
prefix: "${6}"
desc: <<EOD
This is a test
EOD
EOF
}

SUBCMD=$1
shift
${SUBCMD} $*
