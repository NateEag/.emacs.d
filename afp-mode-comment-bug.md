Right now, if you fill a comment of the form

  /**
   *      This is a paragraph that goes for a way and then eventually should wrap to a second line.
   */

it does not properly indent the second line up to the first.

Research suggests afp-only-fill-comments uses fill-comment-paragraph, which
seems to work with //-style comments, but not C-style comments, I think due to
comment-start being // in js2-mode.

js-mode itself uses js-c-fill-paragraph when you press M-q - it's the value of
fill-paragraph-function.

So what if we hack the comment-filling code to use it?

Why, that works just beautifully.

I am currently running with a change that makes the only-fill-comments mode
just suppress filling unless we're in a comment.

As I suspected, though, that fails in modes whose fill logic does more than
just comments.

We could probably work around that by narrowing to the current comment before
filling takes place, via save-restriction.

Finding the current comment's extent is not quite simple, alas. There does not
seem to be a built-in function for doing it.

comment-search-{forward,backward} finds comment starts, but of course multiple
single-line comments in a row confuse it dreadfully. And I see no built-in way
to determine where a comment ends.

So, to go down this route, I would need to write a general function that can
find the bounds of the current comment, for both single- and multi-line
comments. :/

On reflection, that shouldn't be too hard. For multiline comments, just find
the comment's start and the comment's end.

For single-line comments, from the current line, work backwards until you find
a line that doesn't contain just whitespace or comment syntax. Then work
forward until you find a line that doesn't contain just whitespace/comment
syntax. Maybe require that the comments all be indented to the same depth,
too.

...furthermore, if I actually got this working generically, I could finally
have my stupid 'comment' text object for evil.

http://emacs.stackexchange.com/a/19711 claims the general feature for both
single- and multi-line comments exists, and points to the Emacs docs, so yay.

I hacked this up a few weeks ago so in theory I just need to tell my copy of
aggressive-fill-paragraph mode to use it in restricting fill operations.

...holy crap. I just discovered evidence that fill-comment-paragraph is
supposed to narrow to the comment, and just fails to. Conceptually, it
certainly seems like it ought to.

Line 932 of fill.el.gz in my build of Emacs has a comment declaring that as
the intent, while line 1026 says explicitly "Don't fill with narrowing".

I guess I should ask about this on emacs-devel (if git blame doesn't shed any
light).

If this is actually the case, then I spent hours of my life duplicating logic
that was already supposed to be in comment-fill-paragraph. :/

Ah well, I did learn a fair bit from it.
