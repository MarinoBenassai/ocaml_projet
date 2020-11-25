for i in *.dot; do
    [ -f "$i" ] || break
    dot -Tsvg $i > $i.svg
done

