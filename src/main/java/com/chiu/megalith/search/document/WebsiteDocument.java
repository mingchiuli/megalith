package com.chiu.megalith.search.document;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.data.annotation.Id;
import org.springframework.data.elasticsearch.annotations.DateFormat;
import org.springframework.data.elasticsearch.annotations.Document;
import org.springframework.data.elasticsearch.annotations.Field;
import org.springframework.data.elasticsearch.annotations.FieldType;

import java.io.Serializable;
import java.time.ZonedDateTime;

/**
 * @author mingchiuli
 * @create 2022-01-29 2:57 PM
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Document(indexName = "websiteinfo")
public class WebsiteDocument implements Serializable {

    @Id
    @Field(type = FieldType.Text)
    private String id;

    @Field(type = FieldType.Keyword)
    private Integer status;

    @Field(type = FieldType.Text, searchAnalyzer = "ik_smart", analyzer = "ik_max_word")
    private String title;

    @Field(type = FieldType.Text, searchAnalyzer = "ik_smart", analyzer = "ik_max_word")
    private String description;

    @Field(type = FieldType.Text)
    private String link;

    @Field(type = FieldType.Date, format = DateFormat.date_optional_time)
    private ZonedDateTime created;

}
