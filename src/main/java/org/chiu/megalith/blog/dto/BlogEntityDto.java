package org.chiu.megalith.blog.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;


@Data
@AllArgsConstructor
@Builder
@NoArgsConstructor
public class BlogEntityDto implements Serializable {

    private String id;

    private String title;

    private String description;

    private String content;

    private String status;

    private String link;

    private String version;
}
