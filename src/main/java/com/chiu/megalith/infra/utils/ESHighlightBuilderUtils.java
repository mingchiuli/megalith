package com.chiu.megalith.infra.utils;

import org.springframework.data.elasticsearch.core.query.HighlightQuery;
import org.springframework.data.elasticsearch.core.query.highlight.Highlight;
import org.springframework.data.elasticsearch.core.query.highlight.HighlightField;
import org.springframework.data.elasticsearch.core.query.highlight.HighlightParameters;

import java.util.Arrays;

/**
 * @author mingchiuli
 * @create 2023-03-30 9:20 pm
 */
public class ESHighlightBuilderUtils {
    public static final HighlightQuery blogHighlightQueryOrigin = new HighlightQuery(
            new Highlight(
                    new HighlightParameters
                            .HighlightParametersBuilder()
                            .withPreTags("<b style='color:red'>")
                            .withPostTags("</b>")
                            .build(),
                    Arrays.asList(
                            new HighlightField("title"),
                            new HighlightField("description"),
                            new HighlightField("content"))),
            null);

    public static final HighlightQuery blogHighlightQuerySimple = new HighlightQuery(
            new Highlight(
                    new HighlightParameters
                            .HighlightParametersBuilder()
                            .withPreTags("<b style='color:red'>")
                            .withPostTags("</b>")
                            .withNumberOfFragments(1)
                            .withFragmentSize(5)
                            .build(),
                    Arrays.asList(
                            new HighlightField("title"),
                            new HighlightField("description"),
                            new HighlightField("content"))),
            null);

    public static final HighlightQuery websiteHighlightQuery = new HighlightQuery(
            new Highlight(
                    new HighlightParameters
                            .HighlightParametersBuilder()
                            .withPreTags("<b style='color:red'>")
                            .withPostTags("</b>")
                            .build(),
                    Arrays.asList(
                            new HighlightField("title"),
                            new HighlightField("description"))
            ), null);
}
