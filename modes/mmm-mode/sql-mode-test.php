<?php

/* @brief A simple test case to check the highlighting of SQL in PHP files.
 *
 * I doubt I'll get this working, but if I do, it'll be quite the nifty thing.
 * It looks like there's a promising start on this from someone who had to work
 * with Oracle's Pro*C.
 */

function do_nothing() {
    $sql = "SELECT foo, baz, bar
			-- This is a comment, but that only works out if we're wrapped in
            -- double quotes.
            FROM SomeTable
                 JOIN SomeOtherTable
                      ON SomeTable.id = SomeOtherTable.some_table_id
";

    $query = "SELECT
			      *
			  FROM
			      Foobar
				  JOIN Foobaz
				      ON Foobar.foo = Foobaz.faz
			  WHERE Foobaz.faz IS NOT NULL";
}
