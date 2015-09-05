NR_CMDS=$1
NR_PROCS=$2
NR_IDS=$3
NR_KEYS=$4

CMDS=(ACCUMTW)
nr_cmds=${#CMDS[@]}

run(){
    local ids=$1
    local keys=$2
    local cmds=$3
    for i in `seq $cmds`; do
        id=$(( RANDOM % ids ))
        key=$(( RANDOM % keys ))
        cmd_idx=$(( RANDOM % nr_cmds ))
        cmd=${CMDS[$cmd_idx]}
        value=$(( RANDOM % 100 ))
        cmd_go $cmd $key $id $value
    done
}

cmd_go(){
    case $1 in
        GETTW)
            call "$1 $2 $3"
            ;;
        GET)
            call "$1 $2 $3"
            ;;
        ACCUMTW)
            call "$1 $2 $3 $4"
            ;;
        ACCUM)
            call "$1 $2 $3 $4"
            ;;
        *)
            call "$1 $2"
    esac
}

call(){
    echo $1
    echo $1 | nc localhost 4242
}

for i in `seq 0 $NR_KEYS`; do
    call "DECLTW $i Minute"
    call "DECLTW $i Hour"
    call "DECLTW $i Day"
done

for i in `seq $NR_PROCS`; do
    run $NR_IDS $NR_KEYS $NR_CMDS&
done
wait
