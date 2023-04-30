package com.chiu.megalith.blog.entity;

import jakarta.persistence.*;
import lombok.*;
import org.hibernate.Hibernate;
import org.hibernate.annotations.DynamicUpdate;
import org.hibernate.annotations.GenericGenerator;

import java.io.Serial;
import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.Objects;

/**
 * @author mingchiuli
 * @create 2022-11-27 12:56 am
 */
@Entity
@Getter
@Setter
@ToString
@AllArgsConstructor
@RequiredArgsConstructor
@DynamicUpdate
@Builder
@Table(name ="m_blog",
        indexes = {@Index(columnList = "created"), @Index(columnList = "user_id"),
                @Index(name = "IDX_ID_USERID", columnList = "id"), @Index(name = "IDX_ID_USERID", columnList = "user_id"),
                @Index(name = "IDX_ID_STATUS", columnList = "id"), @Index(name = "IDX_ID_STATUS", columnList = "status")})
public class BlogEntity implements Serializable {

    @Serial
    private static final long serialVersionUID =  5450815487641580632L;

    @Id
    @Column(name = "id")
    @GeneratedValue(strategy = GenerationType.AUTO, generator = "native")
    @GenericGenerator(name = "native", strategy = "native")
    private Long id;

    @Column(name = "user_id")
    private Long userId;

    @Column(name = "title")
    private String title;

    @Column(name = "description")
    private String description;

    @Column(name = "content")
    private String content;

    @Column(name = "created")
    private LocalDateTime created;

    @Column(name = "status")
    private Integer status;

    @Column(name = "link")
    private String link;

    @Column(name = "read_count")
    private Long readCount;

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || Hibernate.getClass(this) != Hibernate.getClass(o)) return false;
        BlogEntity that = (BlogEntity) o;
        return id != null && Objects.equals(id, that.id);
    }

    @Override
    public int hashCode() {
        return getClass().hashCode();
    }

    public BlogEntity(Long id,
                      String title,
                      String description,
                      LocalDateTime created,
                      String link) {
        this.id = id;
        this.title = title;
        this.description = description;
        this.created = created;
        this.link = link;
    }
}
