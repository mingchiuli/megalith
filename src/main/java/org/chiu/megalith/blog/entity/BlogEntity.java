package org.chiu.megalith.blog.entity;

import jakarta.persistence.*;
import lombok.*;
import org.hibernate.annotations.DynamicUpdate;
import org.hibernate.annotations.GenericGenerator;

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
@NoArgsConstructor
@DynamicUpdate
@Builder
@Table(name ="m_blog",
        indexes = {@Index(columnList = "created"), @Index(columnList = "user_id"), @Index(columnList = "id,user_id")})
public class BlogEntity {

    @Id
    @Column(name = "id")
    @GeneratedValue(strategy = GenerationType.AUTO, generator = "native")
    @GenericGenerator(name = "native")
    private Long id;

    @Column(name = "user_id")
    private Long userId;

    @Column(name = "title")
    private String title;

    @Column(name = "description")
    private String description;

    @Column(name = "content", length = 65535)
    private String content;

    @Column(name = "created")
    private LocalDateTime created;

    @Column(name = "updated")
    private LocalDateTime updated;

    @Column(name = "status")
    private Integer status;

    @Column(name = "link")
    private String link;

    @Column(name = "read_count")
    private Long readCount;

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        BlogEntity that = (BlogEntity) o;

        if (!Objects.equals(id, that.id)) return false;
        if (!Objects.equals(userId, that.userId)) return false;
        if (!Objects.equals(title, that.title)) return false;
        if (!Objects.equals(description, that.description)) return false;
        if (!Objects.equals(content, that.content)) return false;
        if (!Objects.equals(created, that.created)) return false;
        if (!Objects.equals(updated, that.updated)) return false;
        if (!Objects.equals(status, that.status)) return false;
        if (!Objects.equals(link, that.link)) return false;
        return Objects.equals(readCount, that.readCount);
    }

    @Override
    public int hashCode() {
        return getClass().hashCode();
    }

    public BlogEntity(Long id, String title, String description, LocalDateTime created, String link) {
        this.id = id;
        this.title = title;
        this.description = description;
        this.created = created;
        this.link = link;
    }
}
