package org.chiu.megalith.coop.vo;


import lombok.Data;

@Data
public class BlogEntityVo {

    private Long id;

    private String title;

    private String description;

    private String content;

    private Integer status;

    private String link;
}
