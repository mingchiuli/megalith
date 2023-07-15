package org.chiu.megalith.manage.entity;

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
 * @create 2022-11-27 11:23 am
 */
@Getter
@Setter
@ToString
@AllArgsConstructor
@Builder
@RequiredArgsConstructor
@Entity
@DynamicUpdate
@Table(name ="m_role",
        indexes = {@Index(columnList = "code"), @Index(columnList = "name")},
        uniqueConstraints = {@UniqueConstraint(columnNames = "code"), @UniqueConstraint(columnNames = "name")})
public class RoleEntity implements Serializable {

    @Serial
    private static final long serialVersionUID = 1843749233749274L;

    @Id
    @Column(name = "id")
    @GeneratedValue(strategy = GenerationType.AUTO, generator = "native")
    @GenericGenerator(name = "native")
    private Long id;

    @Column(name = "name")
    private String name;

    @Column(name = "code")
    private String code;

    @Column(name = "remark")
    private String remark;

    @Column(name = "created")
    private LocalDateTime created;

    @Column(name = "updated")
    private LocalDateTime updated;

    @Column(name = "status")
    private Integer status;

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || Hibernate.getClass(this) != Hibernate.getClass(o)) return false;
        RoleEntity that = (RoleEntity) o;
        return id != null && Objects.equals(id, that.id);
    }

    @Override
    public int hashCode() {
        return getClass().hashCode();
    }
}
